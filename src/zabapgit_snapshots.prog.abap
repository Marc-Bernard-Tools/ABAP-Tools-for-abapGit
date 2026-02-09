REPORT zabapgit_snapshots.

********************************************************************************
* abapGit Snapshots
*
* Take regular snapshots of the abapGit storage (table ZABAPGIT). Restore
* complete a snapshot, selected repositories, or user settings.
*
* https://github.com/Marc-Bernard-Tools/ABAP-Tools-for-abapGit
*
* Copyright 2026 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: MIT
********************************************************************************
* MIT License
*
* Copyright (c) 2026 Marc Bernard
*
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in all
* copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
* SOFTWARE.
*
* SPDX-License-Identifier: MIT
********************************************************************************

CONSTANTS c_version TYPE string VALUE '1.0.0' ##NEEDED.

TABLES usr02.

DATA:
  gv_repo_name TYPE c LENGTH 60,
  gi_fe_serv   TYPE REF TO zif_abapgit_frontend_services.

SELECTION-SCREEN BEGIN OF BLOCK sc_header WITH FRAME TITLE sc_title.
  SELECTION-SCREEN:
  SKIP,
  COMMENT /1(77) sc_txt0,
  SKIP,
  COMMENT /1(77) sc_txt1,
  COMMENT /1(77) sc_txt2,
  COMMENT /1(77) sc_txt3.
SELECTION-SCREEN END OF BLOCK sc_header.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK mode WITH FRAME TITLE sc_mode.
  PARAMETERS:
    p_backup RADIOBUTTON GROUP g1 DEFAULT 'X' USER-COMMAND u1,
    p_restor RADIOBUTTON GROUP g1.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 5.
    SELECTION-SCREEN COMMENT 5(20) FOR FIELD p_snapsh MODIF ID m1.
    PARAMETERS p_snapsh TYPE rsddtimestmp MODIF ID m1.
  SELECTION-SCREEN END OF LINE.
  PARAMETERS p_file RADIOBUTTON GROUP g1.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 5.
    SELECTION-SCREEN COMMENT 5(20) FOR FIELD p_path MODIF ID m5.
    PARAMETERS p_path TYPE string LOWER CASE MODIF ID m5.
  SELECTION-SCREEN END OF LINE.
  PARAMETERS p_delete RADIOBUTTON GROUP g1.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 5.
    SELECTION-SCREEN COMMENT 5(20) FOR FIELD p_older MODIF ID m4.
    PARAMETERS p_older TYPE rsddtimestmp MODIF ID m4.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK mode.

SELECTION-SCREEN BEGIN OF BLOCK opt WITH FRAME TITLE sc_opt.
  PARAMETERS:
    p_all  RADIOBUTTON GROUP g2 DEFAULT 'X' USER-COMMAND u2 MODIF ID m1,
    p_repo RADIOBUTTON GROUP g2 MODIF ID m1.
  SELECT-OPTIONS s_repos FOR gv_repo_name LOWER CASE NO INTERVALS MODIF ID m2.
  PARAMETERS p_sett RADIOBUTTON GROUP g2 MODIF ID m1.
  SELECT-OPTIONS s_users FOR usr02-bname MODIF ID m3.
  PARAMETERS:
    p_global RADIOBUTTON GROUP g2 MODIF ID m1,
    p_backgr RADIOBUTTON GROUP g2 MODIF ID m1,
    p_packs  RADIOBUTTON GROUP g2 MODIF ID m1.
SELECTION-SCREEN END OF BLOCK opt.

*------------------------------------------------------------------------------

FORM screen.

  DATA lv_input TYPE abap_bool.

  LOOP AT SCREEN.
    lv_input = abap_true.
    IF screen-group1 = 'M1'.
      lv_input = boolc( p_restor = abap_true OR p_file = abap_true ).
    ENDIF.
    IF screen-group1 = 'M2'.
      lv_input = boolc( ( p_restor = abap_true OR p_file = abap_true ) AND p_repo = abap_true ).
    ENDIF.
    IF screen-group1 = 'M3'.
      lv_input = boolc( ( p_restor = abap_true OR p_file = abap_true ) AND p_sett = abap_true ).
    ENDIF.
    IF screen-group1 = 'M4'.
      lv_input = boolc( p_delete = abap_true ).
    ENDIF.
    IF screen-group1 = 'M5'.
      lv_input = boolc( p_file = abap_true ).
    ENDIF.

    IF lv_input = abap_true.
      screen-input = '1'.
    ELSE.
      screen-input = '0'.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.

*------------------------------------------------------------------------------

FORM backup.

  DATA:
    ls_snap     TYPE zabapgit_snaps,
    lt_data     TYPE zif_abapgit_persistence=>ty_contents,
    lo_zip      TYPE REF TO cl_abap_zip,
    lv_content  TYPE xstring,
    lv_filename TYPE string.

  FIELD-SYMBOLS:
    <ls_data> LIKE LINE OF lt_data.

  lt_data = zcl_abapgit_persistence_db=>get_instance( )->list( ).

  CREATE OBJECT lo_zip.

  LOOP AT lt_data ASSIGNING <ls_data>.
    IF <ls_data>-type = zcl_abapgit_persistence_db=>c_type_repo_csum.
      CONCATENATE <ls_data>-type '_' <ls_data>-value '.txt' INTO lv_filename.
    ELSEIF <ls_data>-type = zcl_abapgit_persistence_db=>c_type_repo_data.
      CONCATENATE <ls_data>-type '_' <ls_data>-value '.json' INTO lv_filename.
    ELSE.
      CONCATENATE <ls_data>-type '_' <ls_data>-value '.xml' INTO lv_filename.
    ENDIF.

    TRY.
        lv_content = zcl_abapgit_convert=>string_to_xstring_utf8( <ls_data>-data_str ).
      CATCH zcx_abapgit_exception.
        MESSAGE 'Convertion error' TYPE 'E' DISPLAY LIKE 'S'.
        STOP.
    ENDTRY.

    lo_zip->add(
      name    = lv_filename
      content = lv_content ).
  ENDLOOP.

  GET TIME STAMP FIELD ls_snap-timestamp.
  CONCATENATE 'abapGit_Snapshot_' sy-datum '_' sy-uzeit '.zip' INTO ls_snap-name.
  ls_snap-zip = lo_zip->save( ).

  INSERT zabapgit_snaps FROM ls_snap.
  IF sy-subrc = 0.
    MESSAGE 'Snapshot successfully saved' TYPE 'S'.
  ELSE.
    MESSAGE 'Save error' TYPE 'E' DISPLAY LIKE 'S'.
  ENDIF.

ENDFORM.

*------------------------------------------------------------------------------

FORM restore.

  CONSTANTS lc_toc_filename TYPE string VALUE '#_Table_of_Content_#.txt'.

  DATA:
    lv_question TYPE string,
    lv_answer   TYPE c LENGTH 1,
    lo_zip      TYPE REF TO cl_abap_zip,
    lv_zip      TYPE xstring,
    lv_filename TYPE string,
    lv_data     TYPE xstring,
    ls_data     TYPE zif_abapgit_persistence=>ty_content,
    lt_data     TYPE zif_abapgit_persistence=>ty_contents.

  FIELD-SYMBOLS:
    <ls_file> LIKE LINE OF lo_zip->files.

  IF p_file IS INITIAL.
    SELECT SINGLE zip FROM zabapgit_snaps INTO lv_zip WHERE timestamp = p_snapsh.
    IF sy-subrc <> 0.
      MESSAGE 'Snapshot not found' TYPE 'E' DISPLAY LIKE 'S'.
      STOP.
    ENDIF.
  ELSE.
    TRY.
        lv_zip = gi_fe_serv->file_upload( p_path ).
      CATCH zcx_abapgit_exception.
        MESSAGE 'File upload error' TYPE 'E' DISPLAY LIKE 'S'.
        STOP.
    ENDTRY.
  ENDIF.

  CREATE OBJECT lo_zip.

  lo_zip->load(
    EXPORTING
      zip             = lv_zip
    EXCEPTIONS
      zip_parse_error = 1
      OTHERS          = 2 ).
  IF sy-subrc <> 0.
    MESSAGE 'Error loading ZIP file' TYPE 'E' DISPLAY LIKE 'S'.
    STOP.
  ENDIF.

  LOOP AT lo_zip->files ASSIGNING <ls_file> WHERE name <> lc_toc_filename.
    CLEAR ls_data.
    lv_filename = <ls_file>-name.
    REPLACE '.xml' IN lv_filename WITH ''.
    REPLACE '.txt' IN lv_filename WITH ''.
    REPLACE '.json' IN lv_filename WITH ''.
    IF lv_filename CP 'REPO_CS*'.
      ls_data-type  = lv_filename(7).
      ls_data-value = lv_filename+8(*).
    ELSEIF lv_filename CP 'REPO_DATA*'.
      ls_data-type  = lv_filename(9).
      ls_data-value = lv_filename+10(*).
    ELSE.
      SPLIT lv_filename AT '_' INTO ls_data-type ls_data-value.
    ENDIF.

    TRY.
        zcl_abapgit_persistence_db=>validate_entry_type( ls_data-type ).
      CATCH zcx_abapgit_exception.
        MESSAGE 'Invalid DB entry type. This is not an abapGit Snapshot' TYPE 'E' DISPLAY LIKE 'S'.
        STOP.
    ENDTRY.

    lo_zip->get(
      EXPORTING
        name                    = <ls_file>-name
      IMPORTING
        content                 = lv_data
      EXCEPTIONS
        zip_index_error         = 1
        zip_decompression_error = 2
        OTHERS                  = 3 ).
    IF sy-subrc <> 0.
      MESSAGE 'Error getting file from ZIP' TYPE 'E' DISPLAY LIKE 'S'.
      STOP.
    ENDIF.

    TRY.
        ls_data-data_str = zcl_abapgit_convert=>xstring_to_string_utf8( lv_data ).
      CATCH zcx_abapgit_exception.
        MESSAGE 'Conversion error' TYPE 'E' DISPLAY LIKE 'S'.
        STOP.
    ENDTRY.

    INSERT ls_data INTO TABLE lt_data.
  ENDLOOP.

  CASE abap_true.
    WHEN p_all.
      lv_question = 'storage'.
    WHEN p_repo.
      lv_question = 'repositories'.
      DELETE lt_data WHERE
        type <> zcl_abapgit_persistence_db=>c_type_repo AND
        type <> zcl_abapgit_persistence_db=>c_type_repo_csum AND
        type <> zcl_abapgit_persistence_db=>c_type_repo_data.
      IF s_repos IS NOT INITIAL.
        DELETE lt_data WHERE value NOT IN s_repos.
      ENDIF.
    WHEN p_sett.
      lv_question = 'user settings'.
      DELETE lt_data WHERE type <> zcl_abapgit_persistence_db=>c_type_user.
      IF s_users IS NOT INITIAL.
        DELETE lt_data WHERE value NOT IN s_users.
      ENDIF.
    WHEN p_global.
      lv_question = 'global settings'.
      DELETE lt_data WHERE type <> zcl_abapgit_persistence_db=>c_type_settings.
    WHEN p_backgr.
      lv_question = 'background settings'.
      DELETE lt_data WHERE type <> zcl_abapgit_persistence_db=>c_type_background.
    WHEN p_packs.
      lv_question = 'package settings'.
      DELETE lt_data WHERE type <> zcl_abapgit_persistence_db=>c_type_packages.
  ENDCASE.

  IF lt_data IS INITIAL.
    MESSAGE 'Nothing to restore' TYPE 'S'.
    STOP.
  ENDIF.

  TRY.
      lv_question = |abapGit *** { lv_question } *** will be deleted and overwritten! Continue?'|.

      lv_answer = zcl_abapgit_ui_factory=>get_popups( )->popup_to_confirm(
        iv_titlebar              = 'Warning'
        iv_text_question         = lv_question
        iv_text_button_1         = 'Restore'
        iv_icon_button_1         = 'ICON_IMPORT'
        iv_text_button_2         = 'Cancel'
        iv_icon_button_2         = 'ICON_CANCEL'
        iv_default_button        = '2'
        iv_display_cancel_button = abap_false ).
    CATCH zcx_abapgit_exception.
      MESSAGE 'Popup error' TYPE 'E' DISPLAY LIKE 'S'.
      STOP.
  ENDTRY.

  IF lv_answer <> '1'.
    MESSAGE 'Restore cancelled' TYPE 'S'.
    STOP.
  ENDIF.

  LOOP AT lt_data INTO ls_data.
    TRY.
        zcl_abapgit_persistence_db=>get_instance( )->delete(
          iv_type  = ls_data-type
          iv_value = ls_data-value ).

        zcl_abapgit_persistence_db=>get_instance( )->add(
          iv_type  = ls_data-type
          iv_value = ls_data-value
          iv_data  = ls_data-data_str ).

      CATCH zcx_abapgit_exception.
        MESSAGE 'Storage error' TYPE 'E' DISPLAY LIKE 'S'.
        STOP.
    ENDTRY.

  ENDLOOP.

  MESSAGE 'Restore successfully completed' TYPE 'S'.

ENDFORM.

*------------------------------------------------------------------------------

FORM delete.

  DATA:
    lv_question TYPE string,
    lv_answer   TYPE c LENGTH 1.

  TRY.
      lv_question = |abapGit snapshots older than { p_older } will be deleted! Continue?|.

      lv_answer = zcl_abapgit_ui_factory=>get_popups( )->popup_to_confirm(
        iv_titlebar              = 'Warning'
        iv_text_question         = lv_question
        iv_text_button_1         = 'Delete'
        iv_icon_button_1         = 'ICON_DELETE'
        iv_text_button_2         = 'Cancel'
        iv_icon_button_2         = 'ICON_CANCEL'
        iv_default_button        = '2'
        iv_display_cancel_button = abap_false ).
    CATCH zcx_abapgit_exception.
      MESSAGE 'Popup error' TYPE 'E' DISPLAY LIKE 'S'.
      STOP.
  ENDTRY.

  IF lv_answer <> '1'.
    MESSAGE 'Deletion cancelled' TYPE 'S'.
    STOP.
  ENDIF.

  DELETE FROM zabapgit_snaps WHERE timestamp < p_older.
  IF sy-subrc = 0.
    MESSAGE 'Snapshots successfully deleted' TYPE 'S'.
  ELSE.
    MESSAGE 'Deletion error' TYPE 'E' DISPLAY LIKE 'S'.
  ENDIF.

ENDFORM.

*------------------------------------------------------------------------------

INITIALIZATION.

  sc_title = 'Description'.
  sc_txt0  = 'Snapshots of the abapGit storage'.
  sc_txt1  = '- Backup snapshots to table ZABAPGIT_SNAPS'.
  sc_txt2  = '- Restore complete a snapshot, selected repositories or settings'.
  sc_txt3  = '- Delete snapshots older than timestamp'.
  sc_mode  = 'Mode'.
  sc_opt   = 'Restore Options'.

  gi_fe_serv = zcl_abapgit_ui_factory=>get_frontend_services( ).

AT SELECTION-SCREEN.

  PERFORM screen.

AT SELECTION-SCREEN OUTPUT.

  PERFORM screen.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_snapsh.

  p_snapsh = zcl_abapgit_f4=>snapshot( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.

  TRY.
      p_path = gi_fe_serv->show_file_open_dialog(
        iv_title            = 'Restore abapGit Backup'
        iv_extension        = 'zip'
        iv_default_filename = 'abapGit_Backup_*.zip' ).
    CATCH zcx_abapgit_exception ##NO_HANDLER.
  ENDTRY.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_older.

  p_older = zcl_abapgit_f4=>snapshot( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_repos-low.

  s_repos-low = zcl_abapgit_f4=>repository( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_users-low.

  s_users-low = zcl_abapgit_f4=>user( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_users-high.

  s_users-high = zcl_abapgit_f4=>user( ).

START-OF-SELECTION.

  CASE abap_true.
    WHEN p_backup.
      PERFORM backup.
    WHEN p_restor OR p_file.
      PERFORM backup. " just to be sure ;-)
      PERFORM restore.
    WHEN p_delete.
      PERFORM delete.
  ENDCASE.
