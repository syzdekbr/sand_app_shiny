list(
  bsTooltip("cut_select_table", "Retrieve password by answering Password Retrieval Question",
            "right", options = list(container = "body")),
  bsTooltip("forgot_password", "Retrieve password by answering Password Retrieval Question",
            "right", options = list(container = "body")),
  bsTooltip("logout_button_ui", "Unsaved data will be lost at log-out",
            "left", options = list(container = "body")),
  bsTooltip("full_patient_record", title = "All available uploaded records. 
            Click on row to select one or multiple records.", placement = "above"),
  bsTooltip("selected_patient", title = "Selected records. Click on record to restore or 
            choose all records for multiple file score conversion.", placement = "above"),
  bsTooltip("upload_format", title = "All available uploaded records. 
            Click on row to select one or multiple records.", placement = "above"), 
  bsTooltip("fileIn", title = "Click on row number to select individual record. 
             For options with multiple records, all records showing are chosen.", placement = "above"),
  bsTooltip(
    "restore_session",
    "Click on desired record row number above and click to restore an individual session.",
    "right",
    options = list(container = "body")
  ),
  
  bsTooltip(
    "save_all_records",
    "Select records in Available Records to Selected Record Table to save all records to .xlsx.",
    "right",
    options = list(container = "body")
  )
)