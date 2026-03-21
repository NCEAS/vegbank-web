// VegBank Web Application JavaScript

// Application constants - injected from R
// window.DOWNLOAD_MAX_RECORDS is set inline in ui.R

Shiny.addCustomMessageHandler('openOverlay', function(message) {
  var overlay = document.getElementById('detail-overlay');
  if (overlay) {
    overlay.classList.remove('closed');
    overlay.style.right = '0px';
    overlay.scrollTop = 0;
    document.body.classList.add('overlay-open');
  }
});

Shiny.addCustomMessageHandler('closeOverlay', function(message) {
  var overlay = document.getElementById('detail-overlay');
  if (overlay) {
    overlay.classList.add('closed');
    document.body.classList.remove('overlay-open');
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

// Binds map_zoom and map_center Shiny inputs with debouncing.
window.vbMapBindShinyInputs = function(map, el) {
  var updateTimeout;
  map.on('zoomend', function() {
    clearTimeout(updateTimeout);
    updateTimeout = setTimeout(function() {
      Shiny.setInputValue('map_zoom', map.getZoom(), {priority: 'event'});
    }, 300);
  });

  map.on('moveend', function() {
    var center = map.getCenter();
    clearTimeout(updateTimeout);
    updateTimeout = setTimeout(function() {
      Shiny.setInputValue('map_center', {lat: center.lat, lng: center.lng}, {priority: 'event'});
    }, 300);
  });
};

// Help/instructions button control — adds a square info button above the zoom controls
// (top-left) that opens a Bootstrap popover with usage instructions.
window.vbMapHelpControl = function(map, el, btnInnerHtml, closeIconHtml, contentHtml) {
  var helpBtn = null;
  var helpControl = L.control({position: 'topleft'});
  helpControl.onAdd = function() {
    var container = L.DomUtil.create('div', 'leaflet-bar leaflet-control vb-map-help-control');
    helpBtn = L.DomUtil.create('a', 'vb-map-help-btn vb-help-btn', container);
    helpBtn.href = '#';
    helpBtn.setAttribute('role', 'button');
    helpBtn.setAttribute('title', 'About this map');
    helpBtn.setAttribute('aria-label', 'About this map');
    helpBtn.innerHTML = btnInnerHtml;
    L.DomEvent.disableClickPropagation(container);
    L.DomEvent.on(helpBtn, 'click', L.DomEvent.preventDefault);
    return container;
  };
  helpControl.addTo(map);

  // Move the help control to the top of the top-left stack (above zoom +/-)
  var topLeft = el.querySelector('.leaflet-top.leaflet-left');
  if (topLeft && topLeft.children.length > 1) {
    topLeft.insertBefore(topLeft.lastElementChild, topLeft.firstElementChild);
  }

  if (helpBtn && window.vbHelpButton) {
    window.vbHelpButton(helpBtn, '<strong>Map</strong>', contentHtml, closeIconHtml);
  }
};

// Inject the status <label> into the .dataTables_filter area on table init.
// Called from each concept table's initComplete callback. Uses the Shiny
// output div id (same resolution as registerDataTableMapping) to look up config.
var vbConceptStatusConfigs = {
  plant_table: { urlKey: 'plants_status',      windowVar: 'vbPlantStatus', cssClass: 'vb-plant-status-select', shinyInput: 'plant_status', paginationKey: 'plants_start' },
  comm_table:  { urlKey: 'communities_status', windowVar: 'vbCommStatus',  cssClass: 'vb-comm-status-select',  shinyInput: 'comm_status',  paginationKey: 'communities_start' }
};

window.vbConceptStatusInit = function(nTableWrapper, tableId) {
  var config = vbConceptStatusConfigs[tableId];
  if (!config) return;
  // Guard against double-injection (e.g. if initComplete fires twice on the same wrapper).
  if ($(nTableWrapper).find('.vb-status-label').length) return;
  var validStatuses = window.VB_VALID_CONCEPT_STATUSES || ['current_accepted', 'current', 'accepted', 'any'];
  var urlVal    = new URLSearchParams(window.location.search).get(config.urlKey);
  var storedVal = window[config.windowVar];
  var initVal   = (storedVal && validStatuses.indexOf(storedVal) !== -1)
    ? storedVal
    : (urlVal && validStatuses.indexOf(urlVal) !== -1 ? urlVal : 'current_accepted');
  var $label = $(
    '<label class="vb-status-label">' +
      'Status:' +
      '<select class="' + config.cssClass + ' vb-status-select">' +
        '<option value="current_accepted">Currently Accepted</option>' +
        '<option value="current">Current</option>' +
        '<option value="accepted">Accepted</option>' +
        '<option value="any">Any</option>' +
      '</select>' +
    '</label>'
  );
  $label.find('select').val(initVal);
  $(nTableWrapper).find('.dataTables_filter').after($label[0]);
  $label.find('select').on('change', function() {
    var val = this.value;
    window[config.windowVar] = val;
    var params = new URLSearchParams(window.location.search);
    if (val === 'current_accepted') { params.delete(config.urlKey); } else { params.set(config.urlKey, val); }
    if (params.has(config.paginationKey)) { params.set(config.paginationKey, '0'); }
    var newSearch = params.toString();
    history.replaceState(null, '', newSearch ? '?' + newSearch : window.location.pathname);
    Shiny.setInputValue(config.shinyInput, val, {priority: 'event'});
  });
};

// Update the status select when the server restores the value from URL.
Shiny.addCustomMessageHandler('setCommStatus', function(message) {
  window.vbCommStatus = message.value;
  $('.vb-comm-status-select').val(message.value);
});

Shiny.addCustomMessageHandler('setPlantStatus', function(message) {
  window.vbPlantStatus = message.value;
  $('.vb-plant-status-select').val(message.value);
});

// Shared helper — set up a toggleable info popover on a DT help button.
// Called from each table's DT button init callback.
var _vbClickHandlerRegistered = false;
window.vbHelpButton = function(btn, title, contentHtml, closeIconHtml) {
  if (typeof bootstrap === 'undefined' || !bootstrap.Popover) return;
  // Normalise: DT Buttons passes a jQuery object; map help passes a plain element.
  if (btn && typeof btn.querySelector !== 'function') btn = btn[0];
  if (!btn) return;
  // Guard against re-initialization on the same node (e.g. DataTable redraw).
  if (btn.dataset.vbHelpInit === '1') return;
  var infoSvg = btn.querySelector('svg');
  if (!infoSvg) return;
  // Inject close SVG alongside info SVG; CSS controls visibility via .vb-help-open
  if (closeIconHtml) {
    var closeWrapper = document.createElement('span');
    closeWrapper.className = 'vb-help-close-icon';
    closeWrapper.innerHTML = closeIconHtml;
    btn.appendChild(closeWrapper);
  }
  btn.dataset.vbHelpInit = '1';
  infoSvg.classList.add('vb-help-info-icon');
  var pop = new bootstrap.Popover(btn, {
    trigger: 'manual',
    html: true,
    placement: 'bottom',
    popperConfig: function(defaultConfig) {
      defaultConfig.placement = 'bottom-start';
      return defaultConfig;
    },
    customClass: 'vb-table-help-popover',
    title: title,
    content: contentHtml
  });
  btn.addEventListener('click', function(e) { e.stopPropagation(); pop.toggle(); });
  btn.addEventListener('shown.bs.popover', function() { btn.classList.add('vb-help-open'); });
  btn.addEventListener('hidden.bs.popover', function() { btn.classList.remove('vb-help-open'); });
  if (!_vbClickHandlerRegistered) {
    _vbClickHandlerRegistered = true;
    document.addEventListener('click', function(e) {
      document.querySelectorAll('.vb-help-btn').forEach(function(helpBtn) {
        var instance = bootstrap.Popover.getInstance(helpBtn);
        if (!instance) return;
        var popId = helpBtn.getAttribute('aria-describedby');
        var popEl = popId ? document.getElementById(popId) : null;
        if (!helpBtn.contains(e.target) && (!popEl || !popEl.contains(e.target))) {
          instance.hide();
        }
      });
    });
  }
};

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
    var obCode = btn.data('ob-code');
    var payload = {
      lat: lat,
      lng: lng
    };

    if (code) {
      payload.code = code;
    }

    if (obCode) {
      payload.ob_code = obCode;
    }

    Shiny.setInputValue('show_on_map', payload, {priority: 'event'});
  });

  // Navigate to the Home tab when the VegBank brand/logo is clicked
  $(document).on('click', '.navbar-brand', function(e) {
    e.preventDefault();
    var homeLink = document.querySelector('.navbar-nav [data-value="Home"]');
    if (homeLink) {
      homeLink.click();
    }
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

  // Show "Read more" only on rows where the description is actually clamped
  $(e.target).find('.dt-description').each(function() {
    var readMore = $(this).siblings('.dt-read-more');
    if (readMore.length) {
      readMore.toggle(this.scrollHeight > this.clientHeight + 1);
    }
  });
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
  // Initialize any loading overlays that are visible on page load
  var visibleOverlays = document.querySelectorAll('.loading-overlay[style*="display: flex"]');
  visibleOverlays.forEach(function(overlay) {
    var overlayId = overlay.id;
    var overlayType = overlayId.replace('-loading-overlay', '');
    
    // Initialize config from data attributes
    var messagesJson = overlay.getAttribute('data-messages');
    var completionMsg = overlay.getAttribute('data-completion-message');
    if (messagesJson) {
      loadingOverlays[overlayType] = {
        messages: JSON.parse(messagesJson),
        completionMessage: completionMsg || 'Done!',
        interval: null,
        messageIndex: 0
      };
      
      // Start rotating messages
      rotateLoadingMessage(overlayType);
      loadingOverlays[overlayType].interval = setInterval(function() {
        rotateLoadingMessage(overlayType);
      }, 2500);
    }
  });
  
  var params = new URLSearchParams(window.location.search);
  if(params.get('details_open') === 'true') {
    var overlay = document.getElementById('detail-overlay');
    if (overlay) {
      overlay.classList.remove('closed');
      overlay.style.right = '0px';
      document.body.classList.add('overlay-open');
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

// DETAIL_ICONS and DETAIL_TYPE_LABELS are injected by R from RESOURCE_REGISTRY
// (see ui.R constants_script). Icons are processed at package load time — no
// browser fetches needed.
Shiny.addCustomMessageHandler('updateDetailType', function(message) {
  const type = message.type;

  // Update the sticky banner with the R-injected icon and display label.
  var iconEl = document.getElementById('detail-type-icon');
  var labelEl = document.getElementById('detail-type-label');
  if (iconEl && window.DETAIL_ICONS && window.DETAIL_ICONS[type]) {
    iconEl.innerHTML = window.DETAIL_ICONS[type];
  }
  if (labelEl && DETAIL_TYPE_LABELS[type]) {
    labelEl.textContent = DETAIL_TYPE_LABELS[type];
  }

  const plotCards = document.getElementById('plot-details-cards');
  const communityConceptCards = document.getElementById('community-concept-details-cards');
  const communityClassificationCards = document.getElementById('community-classification-details-cards');
  const projectCards = document.getElementById('project-details-cards');
  const partyCards = document.getElementById('party-details-cards');
  const plantConceptCards = document.getElementById('plant-concept-details-cards');
  const referenceCards = document.getElementById('reference-details-cards');
  const coverMethodCards = document.getElementById('cover-method-details-cards');
  const stratumMethodCards = document.getElementById('stratum-method-details-cards');
  const taxonObservationCards = document.getElementById('taxon-observation-details-cards');

  console.log('Updating detail type to:', type);

  if (plotCards && communityConceptCards && communityClassificationCards &&
      projectCards && partyCards && plantConceptCards && referenceCards && coverMethodCards &&
      stratumMethodCards && taxonObservationCards) {
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
    taxonObservationCards.style.display = 'none';

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
    } else if (type === 'taxon-observation') {
      console.log('Showing taxon observation details');
      taxonObservationCards.style.display = 'block';
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

  if (filteredCount > window.DOWNLOAD_MAX_RECORDS) {
    var formattedCount = filteredCount.toLocaleString();
    var formattedMax = window.DOWNLOAD_MAX_RECORDS.toLocaleString();
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

// ---- Map Search Control ----
// Self-contained Leaflet control for searching by vb_code or author_obs_code.
// Placed in the top-right corner. Sends the query to Shiny and receives
// results via a custom message handler.
window.vbMapSearchControl = function(map, el) {
  var searchControl = L.control({position: 'topright'});

  searchControl.onAdd = function() {
    var container = L.DomUtil.create('div', 'leaflet-bar leaflet-control vb-map-search-control');
    L.DomEvent.disableClickPropagation(container);
    L.DomEvent.disableScrollPropagation(container);

    // Search input row
    var inputRow = L.DomUtil.create('div', 'vb-map-search-row', container);

    var label = L.DomUtil.create('label', 'vb-map-search-label', inputRow);
    label.textContent = 'Search:';

    var inputWrap = L.DomUtil.create('div', 'vb-map-search-input-wrap', inputRow);
    var input = L.DomUtil.create('input', 'vb-map-search-input', inputWrap);
    input.type = 'text';
    input.placeholder = 'by plot VegBank or author code...';
    input.setAttribute('aria-label', 'Search plot by code');

    var clearBtn = L.DomUtil.create('button', 'vb-map-search-clear', inputWrap);
    clearBtn.type = 'button';
    clearBtn.setAttribute('aria-label', 'Clear search');
    clearBtn.innerHTML = '\u00d7';
    clearBtn.style.display = 'none';

    // Disambiguation / no-results list
    var resultsList = L.DomUtil.create('div', 'vb-map-search-results', container);
    resultsList.style.display = 'none';

    function updateClearBtn() {
      clearBtn.style.display = input.value.length > 0 ? '' : 'none';
    }

    function clearResults() {
      resultsList.innerHTML = '';
      resultsList.style.display = 'none';
    }

    function clearAll() {
      input.value = '';
      updateClearBtn();
      clearResults();
      input.focus();
    }

    function doSearch() {
      var query = input.value.trim();
      if (!query) return;
      clearResults();
      input.disabled = true;
      Shiny.setInputValue('map_search_query', {
        query: query,
        ts: Date.now()
      }, {priority: 'event'});
    }

    input.addEventListener('keydown', function(e) {
      if (e.key === 'Enter') { e.preventDefault(); doSearch(); }
      if (e.key === 'Escape') { clearAll(); }
    });
    input.addEventListener('input', updateClearBtn);
    clearBtn.addEventListener('click', clearAll);

    // Store references on the container for the message handler
    container._vbInput = input;
    container._vbResults = resultsList;
    container._vbClearResults = clearResults;
    container._vbMap = map;

    return container;
  };

  searchControl.addTo(map);
};

// Handler for search results sent from the server
Shiny.addCustomMessageHandler('map_search_results', function(message) {
  var controlEl = document.querySelector('.vb-map-search-control');
  if (!controlEl) return;

  var input = controlEl._vbInput;
  var resultsList = controlEl._vbResults;
  var clearResults = controlEl._vbClearResults;
  var map = controlEl._vbMap;

  input.disabled = false;

  if (!message || !message.status) return;

  if (message.status === 'no_data') {
    resultsList.innerHTML = '<div class="vb-map-search-item vb-map-search-info">Map data not loaded yet.</div>';
    resultsList.style.display = 'block';
    return;
  }

  if (message.status === 'none') {
    resultsList.innerHTML = '<div class="vb-map-search-item vb-map-search-info">No matching plots found.</div>';
    resultsList.style.display = 'block';
    return;
  }

  // Fly to a location and show a popup immediately, before the animation starts.
  // We open the popup with autoPan:false so Leaflet does not call panTo/setView
  // internally, which would interrupt the flyTo animation.  The popup sits at
  // the target coordinates and becomes visible as the camera arrives there.
  // `contentNode` must be a DOM node; text is set via textContent to
  // prevent HTML injection from API-sourced field values.
  function flyAndPopup(lat, lng, contentNode) {
    L.popup({ autoPan: false })
      .setLatLng([lat, lng])
      .setContent(contentNode)
      .openOn(map);
    map.flyTo([lat, lng], 18);
  }

  // Wraps a pre-built label string in a DOM node so flyAndPopup can use
  // textContent (XSS-safe). The label text is produced by build_plot_popup_label()
  // in server.R and sent as message.popup_label / m.popup_label.
  function makePopupNode(label) {
    var span = document.createElement('span');
    span.textContent = label;
    return span;
  }

  if (message.status === 'single') {
    clearResults();
    flyAndPopup(message.lat, message.lng,
      makePopupNode(message.popup_label));
    return;
  }

  if (message.status === 'multiple') {
    resultsList.innerHTML = '';
    var header = document.createElement('div');
    header.className = 'vb-map-search-item vb-map-search-info';
    header.textContent = message.matches.length + ' matches \u2014 select one:';
    resultsList.appendChild(header);

    message.matches.forEach(function(m) {
      var item = document.createElement('a');
      item.href = '#';
      item.className = 'vb-map-search-item vb-map-search-match';
      item.textContent = m.author_obs_code + ' (' + m.ob_code + ')';
      item.addEventListener('click', function(e) {
        e.preventDefault();
        clearResults();
        flyAndPopup(m.lat, m.lng,
          makePopupNode(m.popup_label));
      });
      resultsList.appendChild(item);
    });
    resultsList.style.display = 'block';
    return;
  }
});

// Enable/disable the DT download button (target by class to be index-independent)
Shiny.addCustomMessageHandler('setDownloadButtonState', function(message) {
  try {
    var wrapper = document.getElementById('plot_table');
    if (wrapper) {
      var tableEl = $(wrapper).find('table');
      if (!tableEl.length || !$.fn.DataTable.isDataTable(tableEl)) return;
      var table = tableEl.DataTable();
      if (table && table.buttons) {
        if (message.enabled) {
          table.buttons('.vb-plot-download').enable();
        } else {
          table.buttons('.vb-plot-download').disable();
        }
      }
    }
  } catch(e) {
    console.log('Button state error:', e);
  }
});
