#' Shiny UI for Vegbank Web Application
#'
#' Constructs the user interface for browsing vegetation plot data.
#'
#' @param req A Shiny request object.
#' @return A Shiny tag list.
#'
#' @noRd
#' @param req A Shiny request object.
#' @return A Shiny tag list.
#'
#' @noRd
ui <- function(req) {
  shiny::addResourcePath("assets", system.file("shiny/www", package = "vegbankweb"))

  # Ensure Inter font loads from CDN before any CSS
  font_head <- htmltools::tags$head(
    htmltools::tags$link(rel = "preconnect", href = "https://rsms.me/"),
    htmltools::tags$link(rel = "stylesheet", href = "https://rsms.me/inter/inter.css")
  )

  navbar_with_search <- build_navbar()
  overlay <- build_detail_overlay()

  script <- htmltools::tags$script(htmltools::HTML(
    "Shiny.addCustomMessageHandler('openOverlay', function(message) {
      if (document.getElementById('detail-overlay')) {
        document.getElementById('detail-overlay').style.right = '0px';
      }
    });

    Shiny.addCustomMessageHandler('closeOverlay', function(message) {
      var overlay = document.getElementById('detail-overlay');
      if (overlay) {
        overlay.style.right = '-400px';
      }
    });

    Shiny.addCustomMessageHandler('invalidateMapSize', function(message) {
      var mapWidget = HTMLWidgets.find('#map');
      if (mapWidget) {
        var map = mapWidget.getMap();
        if(map) {
          map.invalidateSize();
        }
      }
    });

    // DataTables/Shiny state synchronization overview:
    // 1. DataTables calls `stateLoadCallback` (vegbankLoadTableState) during init,
    //    which pulls values from URL params so pagination/search/order are set
    //    before the first draw.
    // 2. Subsequent user interactions trigger `stateSaveCallback`
    //    (vegbankSaveTableState) which emits `<tableId>_state` inputs back to
    //    Shiny; the server may then update the encoded URL.
    // 3. When the server needs to push state down later (e.g., browser history
    //    navigation) it sends an `applyTableState` custom message, which uses the
    //    same `applyTableState` helper below to drive the DataTables API.

    // Store pending row selections until tables are ready
    var pendingSelections = [];
    var pendingTableStates = [];
    var currentSelection = null; // Track the currently selected VegBank code

    var tableIdToKey = {
      'plot_table': 'plots',
      'plant_table': 'plants',
      'comm_table': 'communities',
      'party_table': 'people',
      'proj_table': 'projects'
    };

    var tableInitialUrlState = {};
    var tableStateLoadComplete = {};
    var dataTableIdToWidgetId = {};
    var widgetIdToDataTableId = {};

    Object.keys(tableIdToKey).forEach(function(id) {
      tableStateLoadComplete[id] = true;
    });

    function resolveWidgetId(tableId) {
      if (!tableId) {
        return null;
      }

      if (Object.prototype.hasOwnProperty.call(tableIdToKey, tableId)) {
        return tableId;
      }

      if (Object.prototype.hasOwnProperty.call(dataTableIdToWidgetId, tableId)) {
        return dataTableIdToWidgetId[tableId];
      }

      return tableId;
    }

    function registerDataTableMapping(settings) {
      if (!settings || !settings.nTable || !settings.sTableId) {
        return null;
      }

      var widgetId = $(settings.nTable).closest('.datatables').attr('id');
      if (!widgetId) {
        return null;
      }

      dataTableIdToWidgetId[settings.sTableId] = widgetId;
      widgetIdToDataTableId[widgetId] = settings.sTableId;

      if (!Object.prototype.hasOwnProperty.call(tableStateLoadComplete, widgetId)) {
        tableStateLoadComplete[widgetId] = true;
      }

      return widgetId;
    }

    function getResolvedTableIdentifiers(settings) {
      if (!settings || !settings.sTableId) {
        return null;
      }

      var widgetId = resolveWidgetId(settings.sTableId);

      return {
        widgetId: widgetId,
        dataTableId: settings.sTableId,
        effectiveId: widgetId || settings.sTableId
      };
    }

    function clearPendingTableStates(tableId) {
      if (!tableId) {
        return;
      }

      var widgetId = resolveWidgetId(tableId) || tableId;
      for (var i = pendingTableStates.length - 1; i >= 0; i--) {
        if (pendingTableStates[i].tableId === widgetId) {
          pendingTableStates.splice(i, 1);
        }
      }
    }

    function enqueuePendingTableState(tableId, state) {
      if (!tableId || !state) {
        return;
      }

      var widgetId = resolveWidgetId(tableId) || tableId;

      for (var i = pendingTableStates.length - 1; i >= 0; i--) {
        if (pendingTableStates[i].tableId === widgetId) {
          pendingTableStates.splice(i, 1);
        }
      }

      pendingTableStates.push({ tableId: widgetId, state: state });
    }

    function getDataTableNode(tableId) {
      if (!tableId) {
        return $();
      }

      var rawNode = $('#' + tableId);
      if (rawNode.length && rawNode.is('table')) {
        return rawNode;
      }

      var widgetId = resolveWidgetId(tableId);
      var widgetNode = widgetId ? $('#' + widgetId) : $();
      if (widgetNode.length) {
        var nestedTable = widgetNode.find('table.dataTable').first();
        if (nestedTable.length) {
          return nestedTable;
        }
        nestedTable = widgetNode.find('table').first();
        if (nestedTable.length) {
          return nestedTable;
        }
      }

      if (rawNode.length && !rawNode.is('table')) {
        var fallback = rawNode.find('table.dataTable').first();
        if (fallback.length) {
          return fallback;
        }
        fallback = rawNode.find('table').first();
        if (fallback.length) {
          return fallback;
        }
      }

      return rawNode;
    }

    function buildDefaultDtState(settings) {
      var columns = Array.isArray(settings.aoColumns)
        ? settings.aoColumns.map(function(col) {
            return {
              visible: col.bVisible !== undefined ? col.bVisible : true,
              search: {
                search: col.sSearch || '',
                smart: true,
                regex: false,
                caseInsensitive: true
              }
            };
          })
        : [];

      return {
        time: Date.now(),
        start: settings._iDisplayStart || 0,
        length: settings._iDisplayLength || 10,
        order: Array.isArray(settings.aaSorting) ? settings.aaSorting.slice() : [],
        search: {
          search: settings.oPreviousSearch ? settings.oPreviousSearch.sSearch || '' : '',
          smart: true,
          regex: false,
          caseInsensitive: true
        },
        columns: columns
      };
    }

    function parseOrderParam(orderParam) {
      if (typeof orderParam !== 'string' || orderParam.length === 0) {
        return [];
      }

      return orderParam.split(',').map(function(token) {
        var parts = token.split(':');
        if (parts.length !== 2) {
          return null;
        }
        var column = parseInt(parts[0], 10);
        var dir = parts[1];
        if (isNaN(column) || !dir) {
          return null;
        }
        dir = dir.toLowerCase() === 'desc' ? 'desc' : 'asc';
        return [column, dir];
      }).filter(Boolean);
    }

    function getTableKeyFromId(tableId) {
      var widgetId = resolveWidgetId(tableId);
      return tableIdToKey[widgetId] || null;
    }

    function getUrlTableState(tableId) {
      if (!tableId) {
        return null;
      }

      var widgetId = resolveWidgetId(tableId);
      var tableKey = getTableKeyFromId(widgetId);
      if (!tableKey) {
        return null;
      }

      var params = new URLSearchParams(window.location.search);
      var startParam = params.get(tableKey + '_start');
      var lengthParam = params.get(tableKey + '_length');
      var orderParam = params.get(tableKey + '_order');
      var searchParam = params.get(tableKey + '_search');

      if (!startParam && !lengthParam && !orderParam && !searchParam) {
        return null;
      }

      var state = {};
      var startValue = parseInt(startParam, 10);
      if (!isNaN(startValue) && startValue >= 0) {
        state.start = startValue;
      }

      var lengthValue = parseInt(lengthParam, 10);
      if (!isNaN(lengthValue) && lengthValue > 0) {
        state.length = lengthValue;
      }

      var orderValue = parseOrderParam(orderParam);
      if (orderValue.length > 0) {
        state.order = orderValue;
      }

      if (typeof searchParam === 'string') {
        state.search = searchParam;
      }

      return state;
    }

    function getInitialUrlState(tableId) {
      if (!tableId) {
        return null;
      }

      var widgetId = resolveWidgetId(tableId);
      if (!widgetId) {
        return null;
      }

      if (!Object.prototype.hasOwnProperty.call(tableInitialUrlState, widgetId)) {
        var parsed = getUrlTableState(widgetId);
        tableInitialUrlState[widgetId] = parsed;

        if (parsed) {
          tableStateLoadComplete[widgetId] = false;
        }
      }

      return tableInitialUrlState[widgetId];
    }

    window.vegbankLoadTableState = function(settings) {
      registerDataTableMapping(settings);

      var tableId = resolveWidgetId(settings && settings.sTableId);
      console.log('vegbankLoadTableState called for table:', tableId);
      if (!tableId) {
        return null;
      }

      var urlState = getInitialUrlState(tableId);
      if (!urlState) {
        tableStateLoadComplete[tableId] = true;
        return null;
      }

      tableStateLoadComplete[tableId] = false;

      var state = buildDefaultDtState(settings);
      var hasOverride = false;

      if (typeof urlState.start === 'number') {
        state.start = urlState.start;
        hasOverride = true;
      }

      if (typeof urlState.length === 'number') {
        state.length = urlState.length;
        hasOverride = true;
      }

      if (Array.isArray(urlState.order) && urlState.order.length > 0) {
        state.order = urlState.order;
        hasOverride = true;
      }

      if (typeof urlState.search === 'string') {
        state.search.search = urlState.search;
        hasOverride = hasOverride || urlState.search.length > 0;
      }

      return hasOverride ? state : null;
    };

    function sanitizeStatePayload(settings, data) {
      if (!data) {
        return null;
      }

      var payload = {
        start: typeof data.start === 'number' ? data.start : 0,
        length: typeof data.length === 'number' ? data.length : (settings && settings._iDisplayLength) || 10,
        order: Array.isArray(data.order) ? data.order : [],
        search: data.search && typeof data.search.search === 'string' ? data.search.search : ''
      };

      return payload;
    }

    window.vegbankSaveTableState = function(settings, data) {
      if (!window.Shiny || typeof Shiny.setInputValue !== 'function') {
        return;
      }

      var tableInfo = getResolvedTableIdentifiers(settings);
      if (!tableInfo) {
        return;
      }

      if (tableStateLoadComplete[tableInfo.effectiveId] === false) {
        return;
      }

      var payload = sanitizeStatePayload(settings, data);
      if (!payload) {
        return;
      }

      clearPendingTableStates(tableInfo.effectiveId);

      var targetInputId = tableInfo.widgetId || tableInfo.dataTableId;
      if (targetInputId) {
        Shiny.setInputValue(targetInputId + '_state', payload, {priority: 'event'});
      }
    };

    function setNavbarDisabled(disabled) {
      var navbar = document.querySelector('.navbar');
      if (!navbar) {
        return;
      }

      navbar.classList.toggle('nav-disabled', Boolean(disabled));

      var navLinks = navbar.querySelectorAll('.navbar-nav .nav-link');
      navLinks.forEach(function(link) {
        if (disabled) {
          link.setAttribute('aria-disabled', 'true');
          link.setAttribute('tabindex', '-1');
          link.classList.add('disabled');
          if (link.tagName === 'BUTTON') {
            link.disabled = true;
          }
        } else {
          link.removeAttribute('aria-disabled');
          link.removeAttribute('tabindex');
          link.classList.remove('disabled');
          if (link.tagName === 'BUTTON') {
            link.disabled = false;
          }
        }
      });

      var toggler = document.querySelector('.navbar-toggler');
      if (toggler) {
        toggler.disabled = Boolean(disabled);
        if (disabled) {
          toggler.setAttribute('aria-disabled', 'true');
        } else {
          toggler.removeAttribute('aria-disabled');
        }
      }
    }

    setNavbarDisabled(true);

    document.addEventListener('DOMContentLoaded', function() {
      setNavbarDisabled(true);
      
      // Delegated event handlers for DataTable action buttons
      $(document).on('click', '.dt-shiny-action', function(e) {
        e.preventDefault();
        var btn = $(this);
        var inputId = btn.data('input-id');
        var value = btn.data('value');
        
        if (inputId && value) {
          Shiny.setInputValue(inputId, value, {priority: 'event'});
        }
      });

      $(document).on('click', '.dt-map-action', function(e) {
        e.preventDefault();
        var btn = $(this);
        var lat = parseFloat(btn.data('lat'));
        var lng = parseFloat(btn.data('lng'));

        if (!isFinite(lat) || !isFinite(lng)) {
          return;
        }

        var code = btn.data('code');
        var payload = {
          lat: lat,
          lng: lng
        };

        if (code) {
          payload.code = code;
        }

        Shiny.setInputValue('show_on_map', payload, {priority: 'event'});
      });
    });

    Shiny.addCustomMessageHandler('setNavInteractivity', function(message) {
      var disabled = Boolean(message && message.disabled);
      setNavbarDisabled(disabled);
    });

    function getDataTableApi(tableId, settings) {
      if (settings && $.fn && $.fn.dataTable) {
        try {
          return new $.fn.dataTable.Api(settings);
        } catch (error) {
          console.warn('Failed to build DataTables API from settings for', tableId, error);
        }
      }

      var tableNode = getDataTableNode(tableId);
      if (tableNode.length === 0 || !$.fn.dataTable || !$.fn.dataTable.isDataTable(tableNode)) {
        return null;
      }

      return tableNode.DataTable();
    }

    function isDataTableReady(table) {
      return Boolean(table && Array.isArray(table.context) && table.context.length > 0);
    }

    function applyTableState(tableId, state, settings) {
      var widgetId = resolveWidgetId(tableId);
      var targetId = widgetId || tableId;
      console.log('Applying state to table:', targetId, state);

      var table = getDataTableApi(targetId, settings);
      if (!table) {
        return false;
      }

      if (!isDataTableReady(table)) {
        console.warn('DataTable API not ready for table:', targetId, 'deferring state application');
        return false;
      }

      var hasChanges = false;

      if (state.search !== undefined && state.search !== null) {
        var targetSearch = String(state.search);
        if (table.search() !== targetSearch) {
          table.search(targetSearch);
          hasChanges = true;
        }
      }

      if (Array.isArray(state.order)) {
        var targetOrder = state.order.map(function(item) {
          return [parseInt(item.column, 10), item.dir];
        }).filter(function(pair) {
          return !isNaN(pair[0]) && pair[1];
        });

        var currentOrder = table.order();
        if (!Array.isArray(currentOrder)) {
          if (currentOrder && typeof currentOrder.toArray === 'function') {
            currentOrder = currentOrder.toArray();
          } else {
            currentOrder = [];
          }
        }
        var orderDiffers = targetOrder.length !== currentOrder.length ||
          targetOrder.some(function(pair, index) {
            var currentPair = currentOrder[index];
            return !currentPair || currentPair[0] !== pair[0] || currentPair[1] !== pair[1];
          });

        if (orderDiffers) {
          table.order(targetOrder);
          hasChanges = true;
        }
      }

      var pageApiAvailable = table.page && typeof table.page === 'function';
      var targetLength = null;
      if (state.length !== undefined && state.length !== null) {
        targetLength = parseInt(state.length, 10);
        if (
          pageApiAvailable &&
          typeof table.page.len === 'function' &&
          !isNaN(targetLength) &&
          table.page.len() !== targetLength
        ) {
          table.page.len(targetLength);
          hasChanges = true;
        }
      }

      var targetPage = null;
      if (state.page !== undefined && state.page !== null) {
        targetPage = parseInt(state.page, 10);
      } else if (state.start !== undefined && state.start !== null && targetLength) {
        targetPage = Math.floor(parseInt(state.start, 10) / targetLength);
      }

      if (pageApiAvailable && targetPage !== null && !isNaN(targetPage)) {
        if (typeof table.page.info === 'function') {
          var pageInfo = table.page.info();
          if (pageInfo && typeof pageInfo.pages === 'number') {
            var maxPageIndex = Math.max(0, pageInfo.pages - 1);
            if (targetPage > maxPageIndex) {
              targetPage = maxPageIndex;
            }
          }
        }
        if (targetPage < 0) {
          targetPage = 0;
        }

        if (table.page() !== targetPage) {
          table.page(targetPage);
          hasChanges = true;
        }
      }

      if (hasChanges) {
        table.draw(false);
      }

      return true;
    }

    Shiny.addCustomMessageHandler('applyTableState', function(message) {
      if (!message || !message.tableId || !message.state) {
        return;
      }

      var widgetId = resolveWidgetId(message.tableId);
      if (!applyTableState(widgetId, message.state)) {
        enqueuePendingTableState(widgetId, message.state);
      }
    });

    $(document).on('init.dt', function(e, settings) {
      if (!settings || !settings.sTableId) {
        return;
      }

      registerDataTableMapping(settings);
      var tableInfo = getResolvedTableIdentifiers(settings);
      if (!tableInfo) {
        return;
      }

      var urlState = getInitialUrlState(tableInfo.effectiveId);
      if (!urlState && !Object.prototype.hasOwnProperty.call(tableStateLoadComplete, tableInfo.effectiveId)) {
        tableStateLoadComplete[tableInfo.effectiveId] = true;
      }
    });

    $(document).on('draw.dt', function(e, settings) {
      var widgetId = resolveWidgetId(settings.sTableId);
      console.log('DataTable draw event for:', widgetId || settings.sTableId);

      // Restore current selection after table redraw
      if (currentSelection) {
        console.log('Restoring current selection after table redraw:', currentSelection);
        attemptRowSelection(currentSelection, false); // false = don't clear current selection
      }

      // Check for any pending selections for this table or any table
      for (var i = pendingSelections.length - 1; i >= 0; i--) {
        var pendingSelection = pendingSelections[i];
        console.log('Processing pending selection:', pendingSelection.vbCode);

        // Try to select the row now that table is ready
        if (attemptRowSelection(pendingSelection.vbCode)) {
          console.log('Successfully processed pending selection for:', pendingSelection.vbCode);
          // Remove from pending list and set as current selection
          currentSelection = pendingSelection.vbCode;
          pendingSelections.splice(i, 1);
        }
      }

      for (var j = pendingTableStates.length - 1; j >= 0; j--) {
        var pendingState = pendingTableStates[j];
        if (pendingState.tableId === widgetId) {
          if (applyTableState(pendingState.tableId, pendingState.state, settings)) {
            pendingTableStates.splice(j, 1);
          }
        }
      }
    });

    // Function to attempt row selection
    function attemptRowSelection(vbCode, clearCurrent) {
      if (clearCurrent !== false) { // Default to true unless explicitly set to false
        console.log('Attempting to select row with VegBank code:', vbCode);
        // Clear all selections from all tables first
        $('table tbody tr').removeClass('selected-entity');
      }

      // Find the button with the specific VegBank code
      var targetButton = $('button').filter(function() {
        var onclick = $(this).attr('onclick');
        return onclick && onclick.includes(\"'\" + vbCode + \"'\");
      });

      console.log('Found', targetButton.length, 'buttons with VegBank code');

      if (targetButton.length > 0) {
        // Get the row containing the button and select it
        var targetRow = targetButton.closest('tr');
        targetRow.addClass('selected-entity');
        console.log('SUCCESS: Selected row for VegBank code', vbCode);
        return true;
      } else {
        console.log('No buttons found for VegBank code', vbCode);
        return false;
      }
    }

    Shiny.addCustomMessageHandler('selectTableRowByCode', function(message) {
      console.log('Received selection request for:', message.vbCode);

      // Try immediate selection first
      if (!attemptRowSelection(message.vbCode)) {
        console.log('Immediate selection failed, adding to pending queue');
        // Add to pending selections if immediate attempt fails
        pendingSelections.push({
          vbCode: message.vbCode,
          timestamp: Date.now()
        });
      } else {
        // Set as current selection if successful
        currentSelection = message.vbCode;
      }
    });

    Shiny.addCustomMessageHandler('clearAllTableSelections', function(message) {
      $('table tbody tr').removeClass('selected-entity');
      currentSelection = null;
      console.log('Cleared all table selections');
    });

    $(document).ready(function() {
      var params = new URLSearchParams(window.location.search);
      if(params.get('details_open') === 'true') {
        if (document.getElementById('detail-overlay')) {
            document.getElementById('detail-overlay').style.right = '0px';
        }
      }
     });

    $(document).on('stateLoaded.dt', function(e, settings, data) {
      var tableInfo = getResolvedTableIdentifiers(settings);
      if (!tableInfo) {
        return;
      }

      tableStateLoadComplete[tableInfo.effectiveId] = true;
      clearPendingTableStates(tableInfo.effectiveId);

      if (!window.Shiny || typeof Shiny.setInputValue !== 'function') {
        return;
      }

      var payload = sanitizeStatePayload(settings, data);
      if (!payload) {
        return;
      }

      var targetInputId = tableInfo.widgetId || tableInfo.dataTableId;
      if (targetInputId) {
        Shiny.setInputValue(targetInputId + '_state', payload, {priority: 'event'});
      }
    });
    Shiny.addCustomMessageHandler('updateDetailType', function(message) {
      const type = message.type;
      const plotCards = document.getElementById('plot-details-cards');
      const communityConceptCards = document.getElementById('community-concept-details-cards');
      const communityClassificationCards = document.getElementById('community-classification-details-cards');
      const taxonObservationCards = document.getElementById('taxon-observation-details-cards');
      const projectCards = document.getElementById('project-details-cards');
      const partyCards = document.getElementById('party-details-cards');
      const plantConceptCards = document.getElementById('plant-concept-details-cards');
      const referenceCards = document.getElementById('reference-details-cards');

      console.log('Updating detail type to:', type);

      if (plotCards && communityConceptCards && communityClassificationCards &&
      taxonObservationCards && projectCards && partyCards && plantConceptCards && referenceCards) {
        // Hide all card types first
        plotCards.style.display = 'none';
        communityConceptCards.style.display = 'none';
        communityClassificationCards.style.display = 'none';
        taxonObservationCards.style.display = 'none';
        projectCards.style.display = 'none';
        partyCards.style.display = 'none';
        plantConceptCards.style.display = 'none';
        referenceCards.style.display = 'none';

        // Show the requested type
        if (type === 'plot-observation') {
          console.log('Showing plot details');
          plotCards.style.display = 'block';
        } else if (type === 'community-concept') {
          console.log('Showing community details');
          communityConceptCards.style.display = 'block';
        } else if (type === 'community-classification') {
          console.log('Showing community classification details');
          communityClassificationCards.style.display = 'block';
        } else if (type === 'taxon-observation') {
          console.log('Showing taxon observation details');
          taxonObservationCards.style.display = 'block';
        } else if (type === 'project') {
          console.log('Showing project details');
          projectCards.style.display = 'block';
        } else if (type === 'party') {
          console.log('Showing party details');
          partyCards.style.display = 'block';
        } else if (type === 'plant-concept') {
          console.log('Showing plant concept details');
          plantConceptCards.style.display = 'block';
        } else if (type === 'reference') {
          console.log('Showing reference details');
          referenceCards.style.display = 'block';
        }
      }
    });
  "
  ))

  htmltools::tagList(font_head, navbar_with_search, overlay, script)
}

#' Custom Bootstrap Theme for Vegbank Web Application
#'
#' Defines a bslib theme with custom rules.
#'
#' @return A Bootstrap theme object.
#'
#' @noRd
custom_theme <- bslib::bs_theme(
  bg = "#FFFFFF",
  fg = "#19201d",
  info = "#2c5443",
  primary = "#72b9a2",
  secondary = "#72b9a2",
  base_font = bslib::font_collection("Inter", "InterVariable", "system-ui", "sans-serif"),
  heading_font = bslib::font_collection("Inter", "InterVariable", "system-ui", "sans-serif"),
  "font-size-base" = "0.875rem"
)
custom_theme <- bslib::bs_add_rules(
  custom_theme,
  ":root {
    font-family: Inter, sans-serif !important;
    font-feature-settings: 'liga' 1, 'calt' 1;
    --bs-font-sans-serif: Inter, sans-serif !important;

    /* Status badge colors */
    --no-status-bg: hsl(204, 6%, 90%);
    --no-status-text: hsl(0, 0%, 20%);
    --accepted-bg: hsl(153, 31%, 79%);
    --accepted-text: hsl(152, 69%, 19%);
    --not-current-bg: hsl(45, 100%, 85%);
    --not-current-text: hsl(45, 94%, 21%);
  }
  @supports (font-variation-settings: normal) {
    :root {
      font-family: InterVariable, sans-serif !important;
      --bs-font-sans-serif: InterVariable, sans-serif !important;
    }
  }

  *, *::before, *::after {
    font-family: Inter, sans-serif !important;
    font-variant-numeric: tabular-nums slashed-zero !important;
    font-feature-settings: 'tnum' 1, 'zero' 1, 'ss01' 1, 'ss02' 1, 'liga' 1, 'calt' 1 !important;
  }
  @supports (font-variation-settings: normal) {
    *, *::before, *::after {
      font-family: InterVariable, sans-serif !important;
    }
  }

  .card-header {
    background-color: #2c5443;
    color: #FFFFFF;
    font-weight: bold;
  }
  .navbar {
      min-height: 56px !important;
      display: flex;
      align-items: center;
  }
  .navbar.nav-disabled {
    pointer-events: none;
  }
  .navbar.nav-disabled .nav-link {
    cursor: not-allowed !important;
    opacity: 0.65;
  }
  .navbar.nav-disabled .navbar-toggler {
    pointer-events: none;
  }
  .navbar-nav {
      display: flex;
      align-items: center;
      height: 100%;
  }
  .navbar-brand {
      color: #2c5443 !important;
      font-weight: bold;
      padding: 0;
      display: flex;
      align-items: center;
  }
  .navbar-brand img {
      height: 30px;
      margin-right: 10px;
  }
  .nav-item {
      display: flex;
      align-items: center;
      height: 100%;
  }
  .navbar-form {
      display: flex;
      align-items: center;
      margin-bottom: 5px;
  }
  .navbar-form .form-group {
      margin: 0;
  }
  .form-control {
      height: 36px;
  }
  .detail-section {
      display: none;
  }
  #community-description p {
      margin-bottom: 0.75rem;
  }
  .dataTables_wrapper table th {
      font-weight: 500 !important;
  }
  .datatables .dataTables_wrapper div.dataTables_info {
      padding-top: 0.75rem;
      font-size: 0.875rem !important;
  }
  .dataTables_wrapper tbody tr.selected-entity,
  table.dataTable tbody tr.selected-entity,
  .table tbody tr.selected-entity {
      background-color: rgba(114, 185, 162, 0.15) !important;
  }
  .dataTables_wrapper tbody tr.selected-entity:hover,
  table.dataTable tbody tr.selected-entity:hover,
  .table tbody tr.selected-entity:hover {
      background-color: rgba(114, 185, 162, 0.25) !important;
  }
"
)

#' Build Navigation Bar for Vegbank UI
#'
#' Constructs and returns the navigation bar to be used in the UI.
#'
#' @return A Shiny tag list representing the navigation bar.
#'
#' @noRd
build_navbar <- function() {
  navbar <- bslib::page_navbar(
    id = "page",
    theme = custom_theme,
    title = htmltools::tags$span(
      htmltools::tags$img(
        src = "assets/logo_vegbank_leaves.svg"
      ),
      "Vegbank"
    ),
    bslib::nav_panel(
      title = "Overview",
      shiny::fluidPage(
        shiny::fluidRow(
          shiny::column(
            12,
            bslib::card(
              bslib::card_header("App Overview"),
              bslib::card_body(shiny::uiOutput("dataSummary"))
            )
          )
        )
      )
    ),
    bslib::nav_panel(
      title = "Map",
      leaflet::leafletOutput("map")
    ),
    bslib::nav_panel(
      title = "Plots",
      shiny::fluidPage(
        DT::dataTableOutput("plot_table"),
      )
    ),
    bslib::nav_panel(
      title = "Plants",
      shiny::fluidPage(
        DT::dataTableOutput("plant_table"),
      )
    ),
    bslib::nav_panel(
      title = "Communities",
      shiny::fluidPage(
        DT::dataTableOutput("comm_table"),
      )
    ),
    bslib::nav_panel(
      title = "People",
      shiny::fluidPage(
        DT::dataTableOutput("party_table"),
      )
    ),
    bslib::nav_panel(
      title = "Projects",
      shiny::fluidPage(
        DT::dataTableOutput("proj_table"),
      )
    ),
    bslib::nav_menu(
      title = "About",
      align = "right",
      bslib::nav_panel(
        title = "FAQ",
        shiny::includeMarkdown(file.path("inst", "shiny", "www", "faq.md"))
      )
    )
  )
}

#' Build Detail Overlay for Vegbank UI
#'
#' Constructs the overlay panel that displays detailed plot information.
#'
#' @return A Shiny tag representing the detail overlay.
#'
#' @noRd
build_detail_overlay <- function() {
  htmltools::tags$div(
    id = "detail-overlay",
    style = "position: fixed; top: 0; right: -400px; width: 400px; height: 100vh; overflow-y: auto;
             background: #fff; border-left: 1px solid #ccc; z-index: 1050; padding:20px;
             transition: right 0.4s;",
    shiny::actionButton("close_overlay", "",
      onclick = "document.getElementById('detail-overlay').style.right='-400px';
                            Shiny.setInputValue('close_details', true, {priority:'event'});",
      class = "btn-close", style = "float:right; margin-bottom:10px;"
    ),
    shiny::fluidRow(
      shiny::column(
        12,
        # Plot Details Cards - wrapped in a div with class for toggling visibility
        htmltools::tags$div(
          id = "plot-details-cards",
          class = "detail-section",
          bslib::card(bslib::card_header("Plot IDs"), shiny::uiOutput("plot_id_details")),
          bslib::card(bslib::card_header("Location"), shiny::uiOutput("location_details")),
          bslib::card(bslib::card_header("Layout"), shiny::uiOutput("layout_details")),
          bslib::card(bslib::card_header("Environment"), shiny::uiOutput("environmental_details")),
          bslib::card(bslib::card_header("Methods"), shiny::uiOutput("methods_details")),
          bslib::card(bslib::card_header("Plot Quality"), shiny::uiOutput("plot_quality_details")),
          bslib::card(bslib::card_header("Communities"), shiny::uiOutput("communities_details")),
          bslib::card(bslib::card_header("Taxa Observed"), shiny::uiOutput("taxa_details"))
        ),

        # Community Concept Details Cards - wrapped in a div with class for toggling visibility
        htmltools::tags$div(
          id = "community-concept-details-cards",
          class = "detail-section",
          bslib::card(bslib::card_header("Community Concept"), shiny::uiOutput("community_concept_name")),
          bslib::card(bslib::card_header("Concept Details"), shiny::uiOutput("community_concept_details")),
          bslib::card(bslib::card_header("Party Perspective"), shiny::uiOutput("community_party_perspective"))
        ),

        # Community Classification Details Cards - wrapped in a div with class for toggling visibility
        htmltools::tags$div(
          id = "community-classification-details-cards",
          class = "detail-section",
          bslib::card(bslib::card_header("Observation Details"), shiny::uiOutput("observation_details")),
          bslib::card(bslib::card_header("Community Interpretation"), shiny::uiOutput("community_interpretation"))
        ),

        # Project Details Cards - wrapped in a div with class for toggling visibility
        htmltools::tags$div(
          id = "project-details-cards",
          class = "detail-section",
          bslib::card(bslib::card_header("Project Name"), shiny::uiOutput("project_name")),
          bslib::card(bslib::card_header("Description"), shiny::uiOutput("project_description")),
          bslib::card(bslib::card_header("Dates"), shiny::uiOutput("project_dates")),
          bslib::card(bslib::card_header("Contributors"), shiny::uiOutput("project_contributors")),
          bslib::card(bslib::card_header("Plot Observation Count"), shiny::uiOutput("project_observations"))
        ),

        # Taxon Observation Details Cards - wrapped in a div with class for toggling visibility
        htmltools::tags$div(
          id = "taxon-observation-details-cards",
          class = "detail-section",
          bslib::card(bslib::card_header("Taxon Name"), shiny::uiOutput("taxon_name")),
          bslib::card(bslib::card_header("Coverage"), shiny::uiOutput("taxon_coverage")),
          bslib::card(bslib::card_header("Aliases"), shiny::uiOutput("taxon_aliases")),
          bslib::card(bslib::card_header("Identifiers"), shiny::uiOutput("taxon_identifiers"))
        ),

        # Party Details Cards - wrapped in a div with class for toggling visibility
        htmltools::tags$div(
          id = "party-details-cards",
          class = "detail-section",
          bslib::card(bslib::card_header("Name"), shiny::uiOutput("party_name")),
          bslib::card(bslib::card_header("Organization"), shiny::uiOutput("party_organization")),
          bslib::card(bslib::card_header("Contact Information"), shiny::uiOutput("party_contact")),
          bslib::card(bslib::card_header("Contributions"), shiny::uiOutput("party_contributions"))
        ),

        # Plant Concept Details Cards - wrapped in a div with class for toggling visibility
        htmltools::tags$div(
          id = "plant-concept-details-cards",
          class = "detail-section",
          bslib::card(bslib::card_header("Plant Concept"), shiny::uiOutput("plant_concept_name")),
          bslib::card(bslib::card_header("Concept Details"), shiny::uiOutput("plant_concept_details")),
          bslib::card(bslib::card_header("Party Perspective"), shiny::uiOutput("plant_party_perspective"))
        ),

        # Reference Details Cards - wrapped in a div with class for toggling visibility
        htmltools::tags$div(
          id = "reference-details-cards",
          class = "detail-section",
          bslib::card(bslib::card_header("Reference"), shiny::uiOutput("reference_summary")),
          bslib::card(bslib::card_header("Identifiers"), shiny::uiOutput("reference_identifiers")),
          bslib::card(bslib::card_header("Publication"), shiny::uiOutput("reference_publication"))
        )
      )
    )
  )
}
