# Plot Table Download Feature - Implementation Summary

## Overview
Implemented a filtered table download feature for the plot table that exports data as a zipped collection of CSV files, respecting all current search and filter criteria.

## Components Implemented

### 1. Core Download Module (`R/download_table_data.R`)
**Key Features:**
- Configurable download system extensible to other tables
- 20,000 record safety limit
- Nested dataframe extraction with foreign key relationships
- ZIP file generation with multiple related CSV files
- Automatic README generation

**Configuration Structure:**
```r
TABLE_DOWNLOAD_CONFIG <- list(
  plot_table = list(
    resource = "plot-observations",
    has_nested = TRUE,
    nested_fields = c("top_taxon_observations", "top_classifications", 
                      "disturbances", "soils"),
    primary_key = "ob_code",
    api_params = list(with_nested = "TRUE", detail = "full", max_taxa = 100000L)
  )
)
```

**Core Functions:**
- `get_table_filter_state()` - Extracts current search and filter parameters
- `has_table_filters()` - Determines if table has active filters
- `fetch_filtered_count()` - Pre-validates download size
- `fetch_filtered_data()` - Retrieves filtered data from API
- `extract_nested_table()` - Converts nested columns to separate tables
- `prepare_csv_tables()` - Splits data into related CSV tables
- `create_download_zip()` - Packages everything into a ZIP file
- `create_table_download_handler()` - Factory function for download handlers

### 2. UI Updates (`R/ui.R`)
**Added:**
- Download button in the Plots tab (upper right corner)
- Button styling (disabled state with reduced opacity)
- shinyjs initialization for enable/disable functionality

**Button Location:**
```r
bslib::nav_panel(
  title = "Plots",
  shiny::fluidPage(
    shiny::uiOutput("plot_filter_alert"),
    htmltools::tags$div(
      class = "d-flex justify-content-end mb-2",
      shiny::downloadButton("download_plot_table", "Download filtered table", ...)
    ),
    DT::dataTableOutput("plot_table")
  )
)
```

### 3. Server Logic (`R/server.R`)
**Added:**
- Reactive observer to enable/disable download button based on filter state
- Download handler that:
  - Checks record count before fetching
  - Shows progress indicator during download preparation
  - Validates data and provides user feedback
  - Respects current search and cross-resource filters

**Implementation:**
```r
# Enable/disable based on filters
shiny::observe({
  has_filters <- has_table_filters("plot_table", input, state)
  if (has_filters) {
    shinyjs::enable("download_plot_table")
  } else {
    shinyjs::disable("download_plot_table")
  }
})

# Download handler
output$download_plot_table <- create_table_download_handler("plot_table", input, state)
```

### 4. Dependencies
**Added to DESCRIPTION:**
- `shinyjs (>= 2.1.0)` - For button enable/disable functionality

**Added to NAMESPACE:**
- `importFrom(shinyjs,enable)`
- `importFrom(shinyjs,disable)`
- `importFrom(utils,write.csv)`

### 5. Comprehensive Tests (`tests/testthat/test_download_table_data.R`)
**Test Coverage:**
- Configuration validation
- Filter state extraction (search + cross-resource filters)
- Nested dataframe extraction with foreign keys
- CSV table preparation
- README generation
- Download limit enforcement

**All 38 tests passing ✓**

## Download File Structure

When a user downloads filtered plot data, they receive a ZIP file containing:

```
vegbank_plots_20260127.zip
├── README.txt              # Explains structure, includes citation
├── main.csv                # Primary plot data (minus nested columns)
├── taxa.csv                # All taxa observations (with ob_code FK)
├── communities.csv         # Community classifications (with ob_code FK)
├── disturbances.csv        # Disturbance data (with ob_code FK)
└── soils.csv              # Soil data (with ob_code FK)
```

**Foreign Key Relationship:**
- All nested tables include `ob_code` as the first column
- Users can join tables using `ob_code` to reconstruct full plot data

## Usage

### User Experience:
1. **No filters:** Download button is disabled (grayed out)
2. **Add search or filter:** Button becomes enabled
3. **Click download:**
   - System checks count (< 20,000 records)
   - Shows progress: "Fetching X records..." → "Processing nested data..." → "Creating ZIP file..."
   - Downloads ZIP with all related data
   - Shows notification: "Downloaded X records with related data."

### Safety Features:
- **20,000 record limit** - Prevents accidentally downloading entire database
- **Count validation** - Checks before fetching to avoid wasting time/bandwidth
- **Error handling** - Graceful fallbacks with user notifications
- **Progress feedback** - Users see what's happening during long operations

## Extensibility

The system is designed to easily add downloads for other tables:

### To Add Download for Another Table:
1. **Add configuration to `TABLE_DOWNLOAD_CONFIG`:**
   ```r
   party_table = list(
     resource = "parties",
     filename_prefix = "vegbank_parties",
     has_nested = FALSE,  # Simple table, no nested data
     api_params = list(detail = "full"),
     primary_key = "py_code"
   )
   ```

2. **Add UI button** to the appropriate tab in `R/ui.R`

3. **Add server logic** in `R/server.R`:
   ```r
   output$download_party_table <- create_table_download_handler("party_table", input, state)
   ```

That's it! The core infrastructure handles the rest.

## Technical Notes

### API Integration:
- Uses `vegbankr::vb_count()` for pre-validation
- Uses `vegbankr::vb_get()` with full detail and nested data
- Fetches ALL taxa (max_taxa = 100000), not just top 5
- Respects search terms and vb_code cross-resource filters

### File Handling:
- Uses `tempfile()` for secure temporary storage
- Supports both `zip::zip()` (preferred) and `utils::zip()` (fallback)
- Automatic cleanup on exit
- Row names removed from CSV exports for cleaner data

### Performance:
- Progress indicator updates at 0%, 20%, 50%, 80%, 100%
- Efficient nested data extraction (single pass)
- Memory-conscious (processes data once, writes directly to files)

## Known Limitations

1. **Download size:** Hard limit of 20,000 records
   - This is intentional to prevent server overload
   - Users can refine filters to get specific subsets

2. **Button state:** Only updates when filters change
   - Relies on reactive observers
   - No manual refresh needed

3. **Search term capture:** Uses DataTables search input
   - Only works when search is in the input state
   - May need adjustment if search behavior changes

## Future Enhancements (Easy to Add)

1. **Format options:** Add CSV-only download (no ZIP) for simple tables
2. **Field selection:** Let users choose which nested tables to include
3. **Date range filtering:** Add date pickers for temporal filtering
4. **Download history:** Track recent downloads for quick re-access
5. **Email delivery:** For very large downloads, email when ready
6. **Batch downloads:** Download multiple filters as separate files

## Testing Recommendations

Before deploying, test:
1. Download with search term only
2. Download with cross-resource filter only
3. Download with both search and filter
4. Attempt download with > 20,000 records (should reject)
5. Verify foreign keys work correctly in CSV files
6. Test with plots that have:
   - No nested data
   - Partial nested data
   - Full nested data

## Files Modified

1. `/R/download_table_data.R` - **NEW** - Core download module
2. `/R/ui.R` - Added download button, shinyjs init
3. `/R/server.R` - Added download handler and button enable/disable logic
4. `/DESCRIPTION` - Added shinyjs dependency
5. `/NAMESPACE` - Added new imports
6. `/tests/testthat/test_download_table_data.R` - **NEW** - Comprehensive tests

All changes are backward compatible and don't affect existing functionality.
