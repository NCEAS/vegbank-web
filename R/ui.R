#' Shiny UI for Vegbank Web Application
#'
#' Constructs the user interface for browsing vegetation plot data.
#'
#' @param req A Shiny request object.
#' @return A Shiny tag list.
#'
#' @noRd
ui <- function(req) {
  # Handle legacy citation URLs: /cite/IDENTIFIER → ?cite=IDENTIFIER
  # Old VegBank had citation URLs like http://vegbank.org/cite/VB.Ob.22743.INW32086
  # This redirect converts path-based citations to query parameter form for server processing.
  path_info <- req$PATH_INFO

  if (!is.null(path_info) && length(path_info) > 0 && grepl("^/cite/", path_info)) {
    identifier <- sub("^/cite/", "", path_info)
    redirect_url <- paste0("/?cite=", utils::URLencode(identifier, reserved = TRUE))

    # Return JavaScript redirect that executes immediately
    # Use jsonlite::toJSON to safely escape the URL for JavaScript context
    safe_url <- jsonlite::toJSON(redirect_url, auto_unbox = TRUE)
    return(shiny::tags$html(
      shiny::tags$head(
        shiny::tags$script(shiny::HTML(sprintf(
          "window.location.replace(%s);",
          safe_url
        )))
      )
    ))
  }

  shiny::addResourcePath("assets", system.file("shiny/www", package = "vegbankweb"))

  # Ensure Inter font loads from CDN before any CSS
  font_head <- htmltools::tags$head(
    htmltools::tags$link(rel = "preconnect", href = "https://rsms.me/"),
    htmltools::tags$link(rel = "stylesheet", href = "https://rsms.me/inter/inter.css")
  )

  navbar <- build_navbar()
  overlay <- build_detail_overlay()
  map_loading_overlay <- build_map_loading_overlay()
  overview_loading_overlay <- build_overview_loading_overlay()
  download_loading_overlay <- build_download_loading_overlay()
  citation_loading_overlay <- build_citation_loading_overlay()

  script <- htmltools::tags$script(htmltools::HTML(paste0(
    "// Application constants - single source of truth from R\n",
    "const DOWNLOAD_MAX_RECORDS = ", DOWNLOAD_MAX_RECORDS, ";\n\n",
    "Shiny.addCustomMessageHandler('openOverlay', function(message) {
      if (document.getElementById('detail-overlay')) {
        document.getElementById('detail-overlay').style.right = '0px';
      }
    });

    Shiny.addCustomMessageHandler('closeOverlay', function(message) {
      var overlay = document.getElementById('detail-overlay');
      if (overlay) {
        // Use responsive close position based on screen width
        var closePosition = window.innerWidth < 768 ? '-100vw' : '-420px';
        overlay.style.right = closePosition;
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

    // Track pending table state application and pending highlight for initial loads
    var pendingTableStates = [];
    var pendingHighlight = null; // { tableId, rowIndex }
    var currentHighlightTableId = null; // Track which table the highlight belongs to
    var currentHighlightRowIndex = null; // Track which row index is highlighted within the table

    // Check for highlight params in URL and set as pending highlight
    (function() {
      var params = new URLSearchParams(window.location.search);
      var hlTable = params.get('hl_table');
      var hlRow = params.get('hl_row');
      if (hlTable && hlRow !== null) {
        var rowNum = parseInt(hlRow, 10);
        if (!isNaN(rowNum)) {
          pendingHighlight = { tableId: hlTable, rowIndex: rowNum };
        }
      }
    })();

    // Track expected start offsets (pagination) per table so we only highlight
    // after the table has actually navigated to the intended page.
    // Exposed on window for access from DataTables initComplete callback.
    var expectedStartByTable = window.expectedStartByTable = {};

    var tableIdToKey = {
      'plot_table': 'plots',
      'plant_table': 'plants',
      'comm_table': 'communities',
      'party_table': 'parties',
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

        if (parsed && typeof parsed.start === 'number') {
          expectedStartByTable[widgetId] = parsed.start;
        }

        if (parsed) {
          tableStateLoadComplete[widgetId] = false;
        }
      }

      return tableInitialUrlState[widgetId];
    }

    window.vegbankLoadTableState = function(settings) {
      registerDataTableMapping(settings);

      var tableId = resolveWidgetId(settings && settings.sTableId);
      console.log('vegbankLoadTableState called for table:', tableId, 'URL:', window.location.search);
      if (!tableId) {
        console.log('vegbankLoadTableState: no tableId, returning null');
        return null;
      }

      // Always read CURRENT URL state, not cached initial state
      // This ensures table re-renders (e.g., after filter changes) use current URL
      var urlState = getUrlTableState(tableId);
      console.log('vegbankLoadTableState: urlState for', tableId, '=', JSON.stringify(urlState));

      // Update expected start for highlight tracking
      if (urlState && typeof urlState.start === 'number') {
        expectedStartByTable[tableId] = urlState.start;
      }

      if (!urlState) {
        tableStateLoadComplete[tableId] = true;
        console.log('vegbankLoadTableState: no urlState, returning null');
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

      console.log('vegbankLoadTableState: returning state with start=', state.start, 'hasOverride=', hasOverride);
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
        var label = btn.data('label'); // For obs_count links
        var rowIndex = null;
        var row = btn.closest('tr');
        if (row && row.length) {
          rowIndex = row.index();
        }

        // Immediately highlight the clicked row
        $('table tbody tr').removeClass('selected-entity');
        row.addClass('selected-entity');
        var containingTable = btn.closest('table');
        var wrapperId = btn.closest('.datatables').attr('id');
        // Use wrapperId (widget ID) preferentially, as it's consistent across page loads
        // The internal DataTables ID (e.g., DataTables_Table_0) can vary
        var rawTableId = (containingTable && containingTable.attr('id')) || null;
        currentHighlightTableId = wrapperId || resolveWidgetId(rawTableId) || rawTableId;
        currentHighlightRowIndex = rowIndex;

        Shiny.setInputValue('row_highlight', {
          tableId: currentHighlightTableId,
          rowIndex: rowIndex,
          timestamp: Date.now()
        }, {priority: 'event'});

        if (inputId && value) {
          // For obs_count links, send both code and label
          if (label) {
            Shiny.setInputValue(inputId, {code: value, label: label}, {priority: 'event'});
          } else {
            Shiny.setInputValue(inputId, value, {priority: 'event'});
          }
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

    // ==================== GENERALIZED LOADING OVERLAY SYSTEM ====================
    // Reusable loading overlay with rotating messages and fade-out animation.
    // Supports multiple overlay types (map, download, etc.) with custom configurations.
    // Messages and completion text are defined in R and passed via data attributes.

    var loadingOverlays = {};

    function rotateLoadingMessage(overlayType) {
      var config = loadingOverlays[overlayType];
      if (!config) return;

      var punElement = document.getElementById(overlayType + '-loading-pun');
      if (punElement) {
        punElement.textContent = config.messages[config.messageIndex];
        config.messageIndex = (config.messageIndex + 1) % config.messages.length;
      }
    }

    function showLoadingOverlay(overlayType, options) {
      var overlay = document.getElementById(overlayType + '-loading-overlay');
      if (!overlay) return;

      // Initialize config from data attributes if not already loaded
      if (!loadingOverlays[overlayType]) {
        var messagesJson = overlay.getAttribute('data-messages');
        var completionMsg = overlay.getAttribute('data-completion-message');
        if (messagesJson) {
          loadingOverlays[overlayType] = {
            messages: JSON.parse(messagesJson),
            completionMessage: completionMsg || 'Done!',
            interval: null,
            messageIndex: 0
          };
        } else {
          return; // No config available
        }
      }

      var config = loadingOverlays[overlayType];
      var titleElement = document.getElementById(overlayType + '-loading-title');
      var punElement = document.getElementById(overlayType + '-loading-pun');

      // Update title if provided
      if (options && options.title && titleElement) {
        titleElement.textContent = options.title;
      }

      // Update detail/count if provided
      if (options && options.detail) {
        var detailElement = document.getElementById(overlayType + '-loading-detail');
        if (detailElement) {
          detailElement.textContent = ' ' + options.detail;
          detailElement.style.display = 'inline';
        }
      }

      // Show overlay
      overlay.style.display = 'flex';
      overlay.style.pointerEvents = 'auto';
      overlay.classList.remove('fade-out');
      overlay.setAttribute('aria-busy', 'true');

      // Start rotating messages
      config.messageIndex = 0;
      rotateLoadingMessage(overlayType);
      if (config.interval) {
        clearInterval(config.interval);
      }
      config.interval = setInterval(function() {
        rotateLoadingMessage(overlayType);
      }, 2500);
    }

    function updateLoadingOverlay(overlayType, options) {
      if (!options) return;

      if (options.detail) {
        var detailElement = document.getElementById(overlayType + '-loading-detail');
        if (detailElement) {
          detailElement.textContent = ' ' + options.detail;
        }
      }

      if (options.title) {
        var titleElement = document.getElementById(overlayType + '-loading-title');
        if (titleElement) {
          titleElement.textContent = options.title;
        }
      }
    }

    function hideLoadingOverlay(overlayType, options) {
      var config = loadingOverlays[overlayType];
      if (!config) return;

      var overlay = document.getElementById(overlayType + '-loading-overlay');
      var punElement = document.getElementById(overlayType + '-loading-pun');

      if (config.interval) {
        clearInterval(config.interval);
        config.interval = null;
      }

      if (overlay && punElement) {
        // Show completion message
        var completionMsg = (options && options.message) || config.completionMessage;
        punElement.textContent = completionMsg;

        setTimeout(function() {
          overlay.style.pointerEvents = 'none';
          overlay.classList.add('fade-out');
          overlay.setAttribute('aria-busy', 'false');
          setTimeout(function() {
            overlay.style.display = 'none';
            overlay.classList.remove('fade-out');
          }, 500);
        }, 500);
      }
    }

    // Shiny message handlers for loading overlays
    Shiny.addCustomMessageHandler('showLoadingOverlay', function(message) {
      showLoadingOverlay(message.type, message);
    });

    Shiny.addCustomMessageHandler('updateLoadingOverlay', function(message) {
      updateLoadingOverlay(message.type, message);
    });

    Shiny.addCustomMessageHandler('hideLoadingOverlay', function(message) {
      hideLoadingOverlay(message.type, message);
    });

    // ==================== END LOADING OVERLAY SYSTEM ====================

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

      if (state && typeof state.start === 'number') {
        expectedStartByTable[targetId] = state.start;
      }

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

      // Restore current highlight after table redraw (e.g., sorting, filtering)
      if (currentHighlightTableId && currentHighlightRowIndex !== null && !pendingHighlight) {
        console.log('Restoring current highlight after table redraw:', currentHighlightTableId, currentHighlightRowIndex);
        attemptRowHighlight(false, currentHighlightTableId, currentHighlightRowIndex);
      }

      // Check for pending highlight for this table
      if (pendingHighlight && pendingHighlight.tableId === widgetId) {
        console.log('Processing pending highlight for table:', widgetId, 'row:', pendingHighlight.rowIndex);
        if (attemptRowHighlight(true, widgetId, pendingHighlight.rowIndex)) {
          console.log('Successfully applied pending highlight');
          pendingHighlight = null;
        }
      }

      // Apply pending table states
      for (var j = pendingTableStates.length - 1; j >= 0; j--) {
        var pendingState = pendingTableStates[j];
        if (pendingState.tableId === widgetId) {
          if (applyTableState(pendingState.tableId, pendingState.state, settings)) {
            pendingTableStates.splice(j, 1);
          }
        }
      }
    });

    // For server-side AJAX tables, try row highlight after data arrives
    $(document).on('xhr.dt', function(e, settings, json, xhr) {
      var widgetId = resolveWidgetId(settings.sTableId);
      console.log('DataTable xhr event for:', widgetId, '- data received, records:', json && json.recordsTotal);

      // Send filtered record count to Shiny for download button state
      if (widgetId === 'plot_table' && json && typeof json.recordsFiltered === 'number') {
        Shiny.setInputValue('plot_table_filtered_count', json.recordsFiltered, {priority: 'event'});
      }

      // After AJAX data is received, retry pending highlight with a small delay
      // Use requestAnimationFrame + setTimeout to ensure DOM has been updated
      if (pendingHighlight && pendingHighlight.tableId === widgetId) {
        requestAnimationFrame(function() {
          setTimeout(function() {
            if (pendingHighlight && pendingHighlight.tableId === widgetId) {
              console.log('Post-XHR highlight attempt for', widgetId);
              if (attemptRowHighlight(true, widgetId, pendingHighlight.rowIndex)) {
                console.log('Applied pending highlight after XHR');
                pendingHighlight = null;
              }
            }
          }, 100);
        });
      }
    });

    // Function to attempt row highlight using the explicit table and row index.
    // Highlighting is solely driven by tableId and rowIndex.
    function attemptRowHighlight(clearCurrent, tableId, rowIndex) {
      if (clearCurrent !== false) { // Default to true unless explicitly set to false
        console.log('Attempting to highlight row tableId=', tableId, 'rowIndex=', rowIndex);
        // Clear all highlights from all tables first
        $('table tbody tr').removeClass('selected-entity');
      }

      // Must have both tableId and rowIndex to highlight
      if (!tableId || rowIndex === undefined || rowIndex === null || isNaN(rowIndex)) {
        console.log('No explicit tableId/rowIndex provided; skipping highlight');
        return false;
      }

      // For DT widgets, the tableId is the widget container ID (e.g., 'plant_table')
      // The actual data rows may be in a scrollBody table, separate from the header table
      var widgetContainer = $('#' + tableId);
      if (!widgetContainer.length) {
        console.log('No widget container found for tableId', tableId);
        return false;
      }

      // Look for rows in scrollBody first (for tables with scroll enabled)
      var scrollBody = widgetContainer.find('.dataTables_scrollBody tbody');
      var allRows;
      if (scrollBody.length && scrollBody.find('tr').length > 0) {
        allRows = scrollBody.find('tr');
        console.log('Found', allRows.length, 'rows in scrollBody for', tableId);
      } else {
        // Fallback: find any tbody rows in the widget
        allRows = widgetContainer.find('table.dataTable tbody tr');
        console.log('Found', allRows.length, 'rows in dataTable for', tableId);
      }

      // Check if rows are actually loaded (not just a Loading placeholder)
      if (allRows.length === 0 || (allRows.length === 1 && allRows.find('td.dataTables_empty').length > 0)) {
        console.log('Table appears to be loading or empty, rows not ready yet');
        return false;
      }

      var numericIndex = Number(rowIndex);
      var indexedRow = allRows.eq(numericIndex);
      if (indexedRow && indexedRow.length) {
        indexedRow.addClass('selected-entity');
        console.log('SUCCESS: Highlighted row by explicit index', numericIndex, 'in table', tableId);
        currentHighlightTableId = tableId;
        currentHighlightRowIndex = numericIndex;
        return true;
      }

      console.log('Row index', numericIndex, 'not found in table', tableId, '(total rows:', allRows.length, ')');
      return false;
    }

    Shiny.addCustomMessageHandler('highlightTableRow', function(message) {
      console.log('Received highlight request for table:', message.tableId, 'row:', message.rowIndex);

      // Try immediate highlight; if it fails, set as pending for retry after table draw
      if (!attemptRowHighlight(true, message.tableId, message.rowIndex)) {
        console.log('Immediate highlight failed; will retry after table draw');
        pendingHighlight = { tableId: message.tableId, rowIndex: message.rowIndex };
      } else {
        pendingHighlight = null;
      }
    });

    Shiny.addCustomMessageHandler('clearAllTableHighlights', function(message) {
      $('table tbody tr').removeClass('selected-entity');
      currentHighlightTableId = null;
      currentHighlightRowIndex = null;
      console.log('Cleared all table highlights');
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

      var api = getDataTableApi(tableInfo.effectiveId, settings);
      var info = api && typeof api.page === 'function' && typeof api.page().info === 'function'
        ? api.page().info()
        : null;
      if (info && typeof info.start === 'number') {
        expectedStartByTable[tableInfo.effectiveId] = info.start;
      }

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
      const projectCards = document.getElementById('project-details-cards');
      const partyCards = document.getElementById('party-details-cards');
      const plantConceptCards = document.getElementById('plant-concept-details-cards');
      const referenceCards = document.getElementById('reference-details-cards');
      const coverMethodCards = document.getElementById('cover-method-details-cards');
      const stratumMethodCards = document.getElementById('stratum-method-details-cards');

      console.log('Updating detail type to:', type);

      if (plotCards && communityConceptCards && communityClassificationCards &&
          projectCards && partyCards && plantConceptCards && referenceCards && coverMethodCards &&
          stratumMethodCards) {
        // Hide all card types first
        plotCards.style.display = 'none';
        communityConceptCards.style.display = 'none';
        communityClassificationCards.style.display = 'none';
        projectCards.style.display = 'none';
        partyCards.style.display = 'none';
        plantConceptCards.style.display = 'none';
        referenceCards.style.display = 'none';
        coverMethodCards.style.display = 'none';
        stratumMethodCards.style.display = 'none';

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
        } else if (type === 'cover-method') {
          console.log('Showing cover method details');
          coverMethodCards.style.display = 'block';
        } else if (type === 'stratum-method') {
          console.log('Showing stratum method details');
          stratumMethodCards.style.display = 'block';
        }
      }
    });

    // Trigger download by using the Shiny download link
    Shiny.addCustomMessageHandler('triggerDownload', function(message) {
      // Client-side validation: check filtered count before initiating download
      // This prevents the browser from recording a failed download attempt
      var filteredCount = Shiny.shinyapp.$inputValues['plot_table_filtered_count'];

      if (filteredCount === null || filteredCount === undefined) {
        console.error('Cannot determine filtered record count');
        hideLoadingOverlay('download');
        Shiny.notifications.show({
          html: 'Unable to determine record count. Please try again.',
          type: 'error',
          duration: 5000
        });
        return;
      }

      if (filteredCount === 0) {
        hideLoadingOverlay('download');
        Shiny.notifications.show({
          html: 'No records match your current filters.',
          type: 'warning',
          duration: 5000
        });
        return;
      }

      if (filteredCount > DOWNLOAD_MAX_RECORDS) {
        var formattedCount = filteredCount.toLocaleString();
        var formattedMax = DOWNLOAD_MAX_RECORDS.toLocaleString();
        hideLoadingOverlay('download');
        Shiny.notifications.show({
          html: 'Download limit exceeded. Your filters match ' + formattedCount + ' records, but the maximum allowed is ' + formattedMax + '. Please refine your search or filters.',
          type: 'warning',
          duration: 10000
        });
        return;
      }

      // All validations passed - show loading overlay and proceed with download
      showLoadingOverlay('download', {
        detail: 'Preparing your download...'
      });

      var link = document.getElementById('download_plot_table');
      if (link && link.href) {
        // Create a temporary visible link and click it
        // This works around browser security restrictions on clicking hidden elements
        var tempLink = document.createElement('a');
        tempLink.href = link.href;
        tempLink.download = '';
        tempLink.style.display = 'none';
        document.body.appendChild(tempLink);
        tempLink.click();
        document.body.removeChild(tempLink);
      }
    });

    // Enable/disable the DT download button
    Shiny.addCustomMessageHandler('setDownloadButtonState', function(message) {
      try {
        var wrapper = document.getElementById('plot_table');
        if (wrapper) {
          var table = $(wrapper).find('table').DataTable();
          if (table && table.buttons) {
            if (message.enabled) {
              table.buttons(0).enable();
            } else {
              table.buttons(0).disable();
            }
          }
        }
      } catch(e) {
        console.log('Button state error:', e);
      }
    });
  "
  )))

  htmltools::tagList(
    font_head,
    navbar,
    overlay,
    map_loading_overlay,
    overview_loading_overlay,
    download_loading_overlay,
    citation_loading_overlay,
    script
  )
}

#' Custom Bootstrap Theme for Vegbank Web Application
#'
#' Defines a bslib theme with custom rules.
#'
#' @return A Bootstrap theme object.
#'
#' @noRd
custom_theme <- bslib::bs_theme(
  bg = "hsl(0, 0%, 100%)",
  fg = "hsl(156, 12%, 11%)",
  info = "hsl(160, 69%, 30%)",
  primary = "hsl(160, 29%, 48%)",
  secondary = "hsl(160, 34%, 59%)",
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

    /* Navbar height - responsive to mobile collapse */
    --navbar-height: 56px;

    /* Vegbank brand colors */
    --vb-green: hsl(153, 31%, 25%);

    /* Status badge colors */
    --no-status-bg: hsl(204, 6%, 90%);
    --no-status-text: hsl(0, 0%, 20%);
    --accepted-bg: hsl(153, 31%, 79%);
    --accepted-text: hsl(152, 69%, 19%);
    --not-current-bg: hsl(45, 100%, 85%);
    --not-current-text: hsl(45, 94%, 21%);
  }
  
  /* Adjust navbar height for mobile when toggler is visible */
  @media (max-width: 767px) {
    :root {
      --navbar-height: 56px;
    }
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
    background-color: var(--vb-green);
    color: #FFFFFF;
    font-weight: bold;
  }
  .navbar {
      min-height: var(--navbar-height);
      display: flex;
      align-items: center;
      position: fixed;
      top: 0;
      left: 0;
      right: 0;
      z-index: 2000;
      background-color: #fff !important;
      border-bottom: 1px solid rgba(40, 70, 94, 0.1) !important;
      box-shadow: 0 2px 8px rgba(0, 0, 0, 0.08);
  }
  
  /* Add top padding to main content to account for fixed navbar */
  .container-fluid.html-fill-item.html-fill-container {
    padding-top: var(--navbar-height) !important;
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
      color: var(--vb-green) !important;
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

  /* DataTables loading indicator customization */
  .dataTables_processing {
      z-index: 1000 !important;
      position: absolute !important;
      top: 50% !important;
      left: 50% !important;
  }

  /* Target the nested divs that create the loading animation */
  .dataTables_processing > div > div {
      background-color: #72B9A2 !important;
  }

  /* Generic loading ellipses animation for all overlays */
  .loading-ellipses {
      display: inline-block;
      position: relative;
      margin-bottom: 1rem;
      width: 9.375rem;
      height: 1.5rem;
  }

  .loading-ellipses > div {
      position: absolute;
      width: 1.5rem;
      height: 1.5rem;
      border-radius: 50%;
      background: #72B9A2;
      animation-timing-function: cubic-bezier(0, 1, 1, 0);
  }

  .loading-ellipses > div:nth-child(1) {
      left: 0.75rem;
      animation: loadingEllipses1 0.6s infinite;
  }

  .loading-ellipses > div:nth-child(2) {
      left: 0.75rem;
      animation: loadingEllipses2 0.6s infinite;
  }

  .loading-ellipses > div:nth-child(3) {
      left: 3.75rem;
      animation: loadingEllipses2 0.6s infinite;
  }

  .loading-ellipses > div:nth-child(4) {
      left: 6.75rem;
      animation: loadingEllipses3 0.6s infinite;
  }

  @keyframes loadingEllipses1 {
    0% { transform: scale(0); }
    100% { transform: scale(1); }
  }

  @keyframes loadingEllipses2 {
    0% { transform: translate(0, 0); }
    100% { transform: translate(3rem, 0); }
  }

  @keyframes loadingEllipses3 {
    0% { transform: scale(1); }
    100% { transform: scale(0); }
  }

  @keyframes fadeOut {
    from { opacity: 1; }
    to { opacity: 0; }
  }

  #map-loading-overlay.fade-out {
    animation: fadeOut 0.5s ease-out forwards;
  }

  /* Download loading overlay */
  #download-loading-overlay {
    position: fixed;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    background-color: rgba(255, 255, 255, 0.95);
    display: flex;
    flex-direction: column;
    justify-content: center;
    align-items: center;
    z-index: 9999;
  }

  #download-loading-overlay.fade-out {
    animation: fadeOut 0.5s ease-out forwards;
  }

  #download-loading-detail {
    margin-top: 0.5rem;
    font-size: 0.9rem;
    color: #666;
  }

  #citation-loading-overlay.fade-out {
    animation: fadeOut 0.5s ease-out forwards;
  }

  /* Detail overlay responsive width */
  #detail-overlay {
    position: fixed;
    top: var(--navbar-height);
    height: calc(100vh - var(--navbar-height));
    overflow-y: auto;
    background: #fff;
    border-left: 1px solid rgba(40, 70, 94, 0.15);
    box-shadow: -4px 0 16px rgba(0, 0, 0, 0.1);
    z-index: 1050;
    padding: 20px;
    transition: right 0.4s ease-in-out;
  }

  /* Mobile: full width */
  @media (max-width: 767px) {
    #detail-overlay {
      right: -100vw;
      width: 100vw;
    }
  }

  /* Desktop: 420px width */
  @media (min-width: 768px) {
    #detail-overlay {
      right: -420px;
      width: 420px;
    }
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
              bslib::card_header("Vegbank Beta 0.2.0"),
              bslib::card_body(shiny::uiOutput("dataSummary"))
            )
          )
        ),
        bslib::layout_columns(
          col_widths = bslib::breakpoints(
            sm = 12,
            md = 6,
            lg = 4
          ),
          bslib::card(
            bslib::card_header("Core Counts"),
            bslib::card_body(
              shiny::uiOutput("core_counts_list")
            )
          ),
          bslib::card(
            bslib::card_header("Recently Updated Projects"),
            bslib::card_body(
              shiny::uiOutput("latest_projects_table")
            )
          ),
          bslib::card(
            bslib::card_header("Top Projects by Observations"),
            bslib::card_body(
              shiny::uiOutput("top_projects_plot")
            )
          ),
          bslib::card(
            bslib::card_header("Most Observed Community Concepts"),
            bslib::card_body(
              shiny::uiOutput("top_communities_plot")
            )
          ),
          bslib::card(
            bslib::card_header("Most Observed Plant Concepts"),
            bslib::card_body(
              shiny::uiOutput("top_plants_plot")
            )
          ),
          bslib::card(
            bslib::card_header("Top Contributors"),
            bslib::card_body(
              shiny::uiOutput("top_contributors_plot")
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
        shiny::uiOutput("plot_filter_alert"),
        # Hidden download button triggered by DT button click
        # Use position absolute and move off-screen instead of display:none
        # so Shiny properly initializes the href attribute
        htmltools::tags$div(
          style = "position: absolute; left: -9999px; top: -9999px;",
          shiny::downloadButton("download_plot_table", "Download")
        ),
        DT::dataTableOutput("plot_table")
      )
    ),
    bslib::nav_panel(
      title = "Plants",
      shiny::fluidPage(
        DT::dataTableOutput("plant_table")
      )
    ),
    bslib::nav_panel(
      title = "Communities",
      shiny::fluidPage(
        shiny::uiOutput("comm_filter_alert"),
        DT::dataTableOutput("comm_table")
      )
    ),
    bslib::nav_panel(
      title = "Parties",
      shiny::fluidPage(
        DT::dataTableOutput("party_table")
      )
    ),
    bslib::nav_panel(
      title = "Projects",
      shiny::fluidPage(
        DT::dataTableOutput("proj_table")
      )
    ),
    bslib::nav_menu(
      title = "About",
      align = "right",
      bslib::nav_panel(
        title = "Getting Started",
        shiny::includeMarkdown(system.file("shiny", "www", "getting_started.md", package = "vegbankweb"))
      ),
      bslib::nav_panel(
        title = "FAQ",
        shiny::includeMarkdown(system.file("shiny", "www", "faq.md", package = "vegbankweb"))
      ),
      bslib::nav_panel(
        title = "Cite",
        shiny::includeMarkdown(system.file("shiny", "www", "cite.md", package = "vegbankweb"))
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
    shiny::actionButton("close_overlay", "",
      onclick = "var overlay = document.getElementById('detail-overlay');
                 var closePos = window.innerWidth < 768 ? '-100vw' : '-420px';
                 overlay.style.right = closePos;
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
          shiny::uiOutput("plot_notification"),
          bslib::card(bslib::card_header("Plot Observation"), shiny::uiOutput("plot_header")),
          bslib::card(bslib::card_header("Author Codes"), shiny::uiOutput("author_code_details")),
          bslib::card(bslib::card_header("Dates"), shiny::uiOutput("date_details")),
          bslib::card(bslib::card_header("Location"), shiny::uiOutput("location_details")),
          bslib::card(bslib::card_header("Layout"), shiny::uiOutput("layout_details")),
          bslib::card(bslib::card_header("Environment"), shiny::uiOutput("environmental_details")),
          bslib::card(bslib::card_header("Methods"), shiny::uiOutput("methods_details")),
          bslib::card(bslib::card_header("Plot Quality"), shiny::uiOutput("plot_quality_details")),
          bslib::card(bslib::card_header("Plot Vegetation"), shiny::uiOutput("plot_vegetation_details")),
          bslib::card(bslib::card_header("Communities"), shiny::uiOutput("communities_details")),
          bslib::card(bslib::card_header("Taxa Observed"), shiny::uiOutput("taxa_details")),
          bslib::card(bslib::card_header("Disturbances"), shiny::uiOutput("disturbances_details")),
          bslib::card(bslib::card_header("Soils"), shiny::uiOutput("soils_details")),
          bslib::card(bslib::card_header("Miscellaneous"), shiny::uiOutput("plot_misc_details"))
        ),

        # Community Concept Details Cards - wrapped in a div with class for toggling visibility
        htmltools::tags$div(
          id = "community-concept-details-cards",
          class = "detail-section",
          bslib::card(bslib::card_header("Community Concept"), shiny::uiOutput("community_concept_header")),
          bslib::card(bslib::card_header("Concept Details"), shiny::uiOutput("community_concept_details")),
          bslib::card(bslib::card_header("Party Perspective"), shiny::uiOutput("community_party_perspective"))
        ),

        # Community Classification Details Cards - wrapped in a div with class for toggling visibility
        htmltools::tags$div(
          id = "community-classification-details-cards",
          class = "detail-section",
          bslib::card(bslib::card_header("Community Classification"), shiny::uiOutput("comm_class_header")),
          bslib::card(bslib::card_header("Classification Details"), shiny::uiOutput("comm_class_details")),
          bslib::card(bslib::card_header("Community Interpretations"), shiny::uiOutput("comm_class_interpretations")),
          bslib::card(bslib::card_header("Contributors"), shiny::uiOutput("comm_class_contributors"))
        ),

        # Project Details Cards - wrapped in a div with class for toggling visibility
        htmltools::tags$div(
          id = "project-details-cards",
          class = "detail-section",
          bslib::card(bslib::card_header("Project"), shiny::uiOutput("project_header")),
          bslib::card(bslib::card_header("Description"), shiny::uiOutput("project_description")),
          bslib::card(bslib::card_header("Dates"), shiny::uiOutput("project_dates")),
          bslib::card(bslib::card_header("Plot Observation Count"), shiny::uiOutput("project_observations")),
          bslib::card(bslib::card_header("Contributors"), shiny::uiOutput("project_contributors"))
        ),

        # Party Details Cards - wrapped in a div with class for toggling visibility
        htmltools::tags$div(
          id = "party-details-cards",
          class = "detail-section",
          bslib::card(bslib::card_header("Party"), shiny::uiOutput("party_header")),
          bslib::card(bslib::card_header("Organization"), shiny::uiOutput("party_organization")),
          bslib::card(bslib::card_header("Contact Information"), shiny::uiOutput("party_contact")),
          bslib::card(bslib::card_header("Contributions"), shiny::uiOutput("party_contributions"))
        ),

        # Plant Concept Details Cards - wrapped in a div with class for toggling visibility
        htmltools::tags$div(
          id = "plant-concept-details-cards",
          class = "detail-section",
          bslib::card(bslib::card_header("Plant Concept"), shiny::uiOutput("plant_concept_header")),
          bslib::card(bslib::card_header("Concept Details"), shiny::uiOutput("plant_concept_details")),
          bslib::card(bslib::card_header("Party Perspective"), shiny::uiOutput("plant_party_perspective"))
        ),

        # Reference Details Cards - wrapped in a div with class for toggling visibility
        htmltools::tags$div(
          id = "reference-details-cards",
          class = "detail-section",
          bslib::card(bslib::card_header("Reference"), shiny::uiOutput("reference_header")),
          bslib::card(bslib::card_header("Identifiers"), shiny::uiOutput("reference_identifiers")),
          bslib::card(bslib::card_header("Publication"), shiny::uiOutput("reference_publication"))
        ),

        # Cover Method Details Cards - wrapped in a div with class for toggling visibility
        htmltools::tags$div(
          id = "cover-method-details-cards",
          class = "detail-section",
          bslib::card(bslib::card_header("Cover Method"), shiny::uiOutput("cover_method_header")),
          bslib::card(bslib::card_header("Details"), shiny::uiOutput("cover_method_details")),
          bslib::card(bslib::card_header("Cover Indexes"), shiny::uiOutput("cover_method_indexes"))
        ),

        # Stratum Method Details Cards - wrapped in a div with class for toggling visibility
        htmltools::tags$div(
          id = "stratum-method-details-cards",
          class = "detail-section",
          bslib::card(bslib::card_header("Stratum Method"), shiny::uiOutput("stratum_method_header")),
          bslib::card(bslib::card_header("Details"), shiny::uiOutput("stratum_method_details")),
          bslib::card(bslib::card_header("Stratum Types"), shiny::uiOutput("stratum_types"))
        )
      )
    )
  )
}

#' Build Loading Overlay for Vegbank UI
#'
#' Constructs a full-screen loading overlay with animated spinner and rotating messages.
#' This is a generalized component that can be used for map loading, downloads, or other
#' long-running operations.
#'
#' @param overlay_type A string identifier for this overlay (e.g., "map", "download").
#'   Used to generate unique element IDs.
#' @param default_title The default title text shown at the top of the overlay.
#' @param messages Character vector of messages to rotate through while loading.
#' @param completion_message Message to display when loading completes.
#' @param show_detail If TRUE, includes a detail line for showing counts or progress.
#'
#' @return A Shiny tag representing the loading overlay.
#'
#' @noRd
build_loading_overlay <- function(overlay_type, default_title, messages, completion_message, show_detail = FALSE) {
  overlay_id <- paste0(overlay_type, "-loading-overlay")
  title_id <- paste0(overlay_type, "-loading-title")
  detail_id <- paste0(overlay_type, "-loading-detail")
  pun_id <- paste0(overlay_type, "-loading-pun")
  ellipses_class <- "loading-ellipses"

  htmltools::tags$div(
    id = overlay_id,
    class = "loading-overlay",
    role = "alert",
    `aria-live` = "polite",
    `aria-busy` = "true",
    `data-messages` = jsonlite::toJSON(messages, auto_unbox = TRUE),
    `data-completion-message` = completion_message,
    style = "display: none; position: fixed; top: var(--navbar-height); left: 0;
             width: 100vw; height: calc(100vh - var(--navbar-height));
             background: rgba(255, 255, 255, 0.98); z-index: 1200;
             justify-content: center; align-items: center; flex-direction: column;",
    htmltools::tags$div(
      class = "loading-content",
      style = "text-align: center; margin-top: -5rem;",
      htmltools::tags$h2(
        id = title_id,
        style = "font-size: 0.875rem; color: var(--no-status-text); margin-bottom: 1.5rem;",
        default_title,
        if (show_detail) {
          htmltools::tags$span(
            id = detail_id,
            style = "font-size: 0.875rem; color: var(--no-status-text); margin-bottom: 1.5rem; display: none;"
          )
        }
      ),
      htmltools::tags$div(
        class = ellipses_class,
        htmltools::tags$div(htmltools::tags$div()),
        htmltools::tags$div(htmltools::tags$div()),
        htmltools::tags$div(htmltools::tags$div()),
        htmltools::tags$div(htmltools::tags$div())
      ),
      htmltools::tags$div(
        id = pun_id,
        style = "font-size: 1rem; color: var(--vb-green); font-weight: 500;"
      )
    )
  )
}

#' Build Map Loading Overlay for Vegbank UI
#'
#' Constructs a full-screen loading overlay for initial map loading with animated
#' spinner and rotating plant puns.
#'
#' @return A Shiny tag representing the map loading overlay.
#'
#' @noRd
build_map_loading_overlay <- function() {
  build_loading_overlay(
    overlay_type = "map",
    default_title = "Loading the map for the first time can take a few seconds. It's busy:",
    messages = c(
      "Planting seeds...",
      "Branching out...",
      "Rooting through the database...",
      "Monitoring mycelial networks...",
      "Disturbing the substrate...",
      "Planning successful succession...",
      "Aggregating observations (there are a lot)...",
      "Braving the brush...",
      "Last bud not leaf..."
    ),
    completion_message = "Go fir launch!"
  )
}

#' Build Overview Loading Overlay for Vegbank UI
#'
#' Constructs a full-screen loading overlay for overview statistics with animated
#' spinner and rotating messages.
#'
#' @return A Shiny tag representing the overview loading overlay.
#'
#' @noRd
build_overview_loading_overlay <- function() {
  build_loading_overlay(
    overlay_type = "overview",
    default_title = "Loading the app can take a few seconds. Hold tight, we're:",
    messages = c(
      "Rooting through the database...",
      "Monitoring mycelial networks...",
      "Disturbing the substrate...",
      "Counting all the plots (there are a lot)...",
      "Surveying the survey data...",
      "Celebrating our top contributors...",
      "Crunching the numbers...",
      "Classifiying communities...",
      "Sorting through plant species...",
      "Compiling project data...",
      "Organizing taxonomic hierarchies..."
    ),
    completion_message = "Take a look!"
  )
}

#' Build Download Loading Overlay for Vegbank UI
#'
#' Constructs a full-screen loading overlay for data downloads with animated
#' spinner and rotating messages.
#'
#' @return A Shiny tag representing the download loading overlay.
#'
#' @noRd
build_download_loading_overlay <- function() {
  build_loading_overlay(
    overlay_type = "download",
    default_title = "Preparing your download:",
    messages = c(
      "Gathering plot observations...",
      "Untangling nested data...",
      "Pressing specimens into CSVs...",
      "Bundling the herbarium...",
      "Zipping up the collection..."
    ),
    completion_message = "Your data is ready!",
    show_detail = TRUE
  )
}

#' Build Citation Loading Overlay for Vegbank UI
#'
#' Constructs a full-screen loading overlay for citation resolution with animated
#' spinner and rotating messages.
#'
#' @return A Shiny tag representing the citation loading overlay.
#'
#' @noRd
build_citation_loading_overlay <- function() {
  build_loading_overlay(
    overlay_type = "citation",
    default_title = "Resolving citation...",
    messages = c(
      "Looking up identifier...",
      "Consulting the archives...",
      "Following the paper trail...",
      "Tracking down that reference...",
      "Dusting off old records..."
    ),
    completion_message = "Citation resolved!"
  )
}
