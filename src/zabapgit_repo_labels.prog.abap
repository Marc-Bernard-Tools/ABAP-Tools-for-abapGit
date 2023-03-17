REPORT zabapgit_repo_labels.

********************************************************************************
* abapGit Repository Labels
*
* A tool for doing mass-maintenance of repository labels. Quickly display, add,
* or remove labels based on free selection of repository name, package, or URL.
*
* https://github.com/Marc-Bernard-Tools/ABAP-Tools-for-abapGit
* https://marcbernardtools.com/
********************************************************************************
* The MIT License (MIT)
*
* Copyright (c) 2023 Marc Bernard
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

CONSTANTS c_version TYPE string VALUE '1.0.0'.

TABLES: tdevc, tdevct.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:
    so_name FOR tdevct-ctext LOWER CASE,
    so_pack FOR tdevc-devclass,
    so_url  FOR tdevct-ctext LOWER CASE.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS:
    p_list   RADIOBUTTON GROUP g1 DEFAULT 'X',
    p_add    RADIOBUTTON GROUP g1,
    p_remove RADIOBUTTON GROUP g1.
  SELECT-OPTIONS:
    so_label FOR tdevct-ctext LOWER CASE NO INTERVALS.
SELECTION-SCREEN END OF BLOCK b2.

DATA gt_repos TYPE zif_abapgit_repo_srv=>ty_repo_list.

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

        IF NOT ( lv_name IN so_name AND lv_package IN so_pack AND lv_url IN so_url ).
          DELETE gt_repos.
        ENDIF.
      ENDLOOP.
    CATCH zcx_abapgit_exception INTO lx_error.
      MESSAGE lx_error TYPE 'E'.
  ENDTRY.
ENDFORM.

FORM list.

  TYPES:
    BEGIN OF ty_alv,
      name    TYPE string,
      package TYPE devclass,
      url     TYPE string,
      labels  TYPE string,
    END OF ty_alv.

  DATA:
    lo_online   TYPE REF TO zcl_abapgit_repo_online,
    li_repo     TYPE REF TO zif_abapgit_repo,
    ls_settings TYPE zif_abapgit_persistence=>ty_repo-local_settings,
    ls_alv      TYPE ty_alv,
    lt_alv      TYPE STANDARD TABLE OF ty_alv WITH DEFAULT KEY.

  LOOP AT gt_repos ASSIGNING FIELD-SYMBOL(<li_repo>).
    ls_settings = <li_repo>->get_local_settings( ).

    CLEAR ls_alv.
    ls_alv-name    = <li_repo>->get_name( ).
    ls_alv-package = <li_repo>->get_package( ).
    IF <li_repo>->is_offline( ) = abap_false.
      lo_online ?= <li_repo>.
      ls_alv-url = lo_online->get_url( ).
    ENDIF.
    ls_alv-labels  = ls_settings-labels.
    INSERT ls_alv INTO TABLE lt_alv.
  ENDLOOP.

  SORT lt_alv.

  CALL FUNCTION 'RSDU_CALL_ALV_TABLE'
    EXPORTING
      i_title   = 'Repository Labels'
      i_ta_data = lt_alv.

ENDFORM.

FORM add_remove.

  DATA:
    li_repo     TYPE REF TO zcl_abapgit_repo,
    ls_settings TYPE zif_abapgit_persistence=>ty_repo-local_settings,
    lv_label    TYPE string.

  FIELD-SYMBOLS:
    <li_repo>  TYPE REF TO zif_abapgit_repo,
    <lv_label> LIKE LINE OF so_label.

  IF so_label IS INITIAL.
    MESSAGE 'Enter at least one label' TYPE 'I'.
    RETURN.
  ENDIF.

  LOOP AT gt_repos ASSIGNING <li_repo>.
    li_repo ?= <li_repo>.

    ls_settings = li_repo->get_local_settings( ).

    SPLIT ls_settings-labels AT ',' INTO TABLE DATA(lt_labels).

    LOOP AT so_label ASSIGNING <lv_label>.
      lv_label = condense( <lv_label>-low ).
      READ TABLE lt_labels TRANSPORTING NO FIELDS WITH KEY table_line = lv_label.
      IF sy-subrc <> 0 AND p_add = abap_true.
        INSERT lv_label INTO TABLE lt_labels.
      ELSEIF sy-subrc = 0 AND p_remove = abap_true.
        DELETE lt_labels WHERE table_line = lv_label.
      ENDIF.
    ENDLOOP.

    CONCATENATE LINES OF lt_labels INTO lv_label SEPARATED BY ','.

    ls_settings-labels = zcl_abapgit_repo_labels=>normalize( lv_label ).

    TRY.
        li_repo->set_local_settings( ls_settings ).
        COMMIT WORK AND WAIT.
      CATCH zcx_abapgit_exception INTO DATA(lx_error).
        MESSAGE lx_error TYPE 'I'.
    ENDTRY.
  ENDLOOP.

ENDFORM.

INITIALIZATION.

  %_so_name_%_app_%-text  = 'Name'.
  %_so_pack_%_app_%-text  = 'Package'.
  %_so_url_%_app_%-text   = 'URL'.
  %_so_label_%_app_%-text = 'Labels'.
  %_p_list_%_app_%-text   = 'Show All Labels'.
  %_p_add_%_app_%-text    = 'Add Labels'.
  %_p_remove_%_app_%-text = 'Remove Labels'.
  %b001000_block_1000     = 'Repository Selection'.
  %b002005_block_1000     = 'Action'.

START-OF-SELECTION.

  PERFORM get.

  IF p_add = abap_true OR p_remove = abap_true.
    PERFORM add_remove.
  ENDIF.

  PERFORM list.
