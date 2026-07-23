REPORT zabapgit_repo_export.

********************************************************************************
* abapGit Repository Export
*
* This is a tool for exporting of a selection of repositories to ZIP files
*
* https://github.com/Marc-Bernard-Tools/ABAP-Tools-for-abapGit
*
* Copyright 2026 abapGit Community
* SPDX-License-Identifier: MIT
********************************************************************************
* The MIT License (MIT)
*
* Copyright (c) 2026 abapGit Community
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
********************************************************************************

CONSTANTS c_version TYPE string VALUE '1.0.0' ##NEEDED.

TABLES: tdevc, tdevct.

SELECTION-SCREEN BEGIN OF BLOCK sc_header WITH FRAME TITLE sc_titl0.
  SELECTION-SCREEN:
  SKIP,
  COMMENT /1(77) sc_txt1,
  SKIP.
SELECTION-SCREEN END OF BLOCK sc_header.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK sc_repo WITH FRAME TITLE sc_titl1.
  SELECT-OPTIONS:
    s_name FOR tdevct-ctext LOWER CASE,
    s_pack FOR tdevc-devclass,
    s_url  FOR tdevct-ctext LOWER CASE.
SELECTION-SCREEN END OF BLOCK sc_repo.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK sc_zip WITH FRAME TITLE sc_titl2.
  PARAMETERS:
    p_folder TYPE string LOWER CASE OBLIGATORY.
SELECTION-SCREEN END OF BLOCK sc_zip.

DATA gt_repos TYPE zif_abapgit_repo_srv=>ty_repo_list.
DATA gi_fe_serv    TYPE REF TO zif_abapgit_frontend_services.

FORM get.

  DATA:
    lo_online  TYPE REF TO zcl_abapgit_repo_online,
    lv_name    TYPE string,
    lv_package TYPE devclass,
    lv_url     TYPE string,
    lx_error   TYPE REF TO zcx_abapgit_exception.

  FIELD-SYMBOLS <li_repo> TYPE REF TO zif_abapgit_repo.

  TRY.
      gt_repos = zcl_abapgit_repo_srv=>get_instance( )->list( ).

      LOOP AT gt_repos ASSIGNING <li_repo>.
        lv_name    = <li_repo>->get_name( ).
        lv_package = <li_repo>->get_package( ).

        IF <li_repo>->is_offline( ) = abap_false.
          lo_online ?= <li_repo>.
          lv_url = lo_online->get_url( ).
        ELSE.
          lv_url = ''.
        ENDIF.

        IF NOT ( lv_name IN s_name AND lv_package IN s_pack AND lv_url IN s_url ).
          DELETE gt_repos.
        ENDIF.
      ENDLOOP.
    CATCH zcx_abapgit_exception INTO lx_error.
      MESSAGE lx_error TYPE 'E'.
  ENDTRY.

ENDFORM.

FORM export.

  DATA:
    lv_sub_folder TYPE string,
    lv_sep        TYPE c LENGTH 1,
    lv_timestamp  TYPE string,
    lv_rc         TYPE i,
    lv_name       TYPE string,
    lv_filename   TYPE string,
    lv_xstr       TYPE xstring,
    lx_error      TYPE REF TO zcx_abapgit_exception.

  FIELD-SYMBOLS:
    <li_repo>  TYPE REF TO zif_abapgit_repo.

  TRY.
      gi_fe_serv->get_file_separator( CHANGING cv_file_separator = lv_sep ).

      " Create a timestamped sub-folder to keep the exports together
      lv_timestamp  = |{ sy-datlo }_{ sy-timlo }|.
      lv_sub_folder = |{ p_folder }{ lv_sep }{ lv_timestamp }|.
      IF gi_fe_serv->directory_exist( lv_sub_folder ) = abap_false.
        gi_fe_serv->directory_create(
          EXPORTING
            iv_directory = lv_sub_folder
          CHANGING
            cv_rc        = lv_rc ).
      ENDIF.

      LOOP AT gt_repos ASSIGNING <li_repo>.

        lv_xstr = zcl_abapgit_zip=>encode_files( <li_repo>->get_files_local( ) ).

        lv_name = <li_repo>->get_package( ).
        TRANSLATE lv_name USING '/#'.
        lv_filename = |{ lv_sub_folder }{ lv_sep }{ lv_name }_{ lv_timestamp }.zip|.

        zcl_abapgit_zip=>save_binstring_to_localfile(
          iv_filename  = lv_filename
          iv_binstring = lv_xstr ).

      ENDLOOP.

      " Open the folder for the user
      gi_fe_serv->execute( iv_document = lv_sub_folder ).

    CATCH zcx_abapgit_exception INTO lx_error.
      MESSAGE lx_error TYPE 'I'.
  ENDTRY.

ENDFORM.

INITIALIZATION.

  sc_titl0 = 'Description'.
  sc_txt1  = 'This is a tool for exporting of a selection of repositories to ZIP files.'.
  sc_titl1 = 'Repository Selection'.
  sc_titl2 = 'Destination'.

  gi_fe_serv = zcl_abapgit_ui_factory=>get_frontend_services( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_folder.

  TRY.
      gi_fe_serv->directory_browse(
        EXPORTING
          iv_window_title    = 'Choose the destination folder for the ZIP files'
        CHANGING
          cv_selected_folder = p_folder ).
    CATCH zcx_abapgit_exception ##NO_HANDLER.
  ENDTRY.

START-OF-SELECTION.

  PERFORM get.

  PERFORM export.
