REPORT zabapgit_starter.

********************************************************************************
* abapGit Starter
*
* Simple tool that lets you define variants for running abapGit. Pick between
* developer or standalone version and starting up with repository list, last
* repository, or a specific repository based on key or SAP package.
*
* https://github.com/Marc-Bernard-Tools/ABAP-Tools-for-abapGit
*
* Copyright 2023 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: MIT
********************************************************************************
* MIT License
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
*
* SPDX-License-Identifier: MIT
********************************************************************************

CONSTANTS c_version TYPE string VALUE '1.0.0' ##NEEDED.

SELECTION-SCREEN BEGIN OF BLOCK sc_header WITH FRAME TITLE sc_titl0.
  SELECTION-SCREEN:
  SKIP,
  COMMENT /1(77) sc_txt0,
  SKIP,
  COMMENT /1(77) sc_txt1,
  COMMENT /1(77) sc_txt2,
  COMMENT /1(77) sc_txt3.
SELECTION-SCREEN END OF BLOCK sc_header.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK sc_version WITH FRAME TITLE sc_titl1.
  PARAMETERS:
    p_devel RADIOBUTTON GROUP g1 DEFAULT 'X',
    p_stand RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK sc_version.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK sc_repo WITH FRAME TITLE sc_titl2.
  PARAMETERS:
    p_list RADIOBUTTON GROUP g2 DEFAULT 'X' USER-COMMAND repo,
    p_last RADIOBUTTON GROUP g2,
    p_repo RADIOBUTTON GROUP g2,
    p_pack TYPE tdevc-devclass MODIF ID pac.
SELECTION-SCREEN END OF BLOCK sc_repo.

FORM screen.

  DATA lv_input TYPE abap_bool.

  LOOP AT SCREEN.
    IF screen-group1 = 'PAC'.
      lv_input = boolc( p_repo = abap_true ).
    ELSE.
      lv_input = abap_true.
    ENDIF.

    IF lv_input = abap_true.
      screen-input = '1'.
    ELSE.
      screen-input = '0'.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.

INITIALIZATION.

  sc_titl0 = 'Description'.
  sc_txt0  = 'Simple tool that lets you define variants for running abapGit'.
  sc_txt1  = '- Save your favorite option as a variant'.
  sc_txt2  = '- Create a transaction for your variant'.
  sc_txt3  = '- Enjoy using a tcode for your favorite repository'.
  sc_titl1 = 'abapGit Version'.
  sc_titl2 = 'Repository Selection'.

AT SELECTION-SCREEN.

  PERFORM screen.

AT SELECTION-SCREEN OUTPUT.

  PERFORM screen.

START-OF-SELECTION.

  DATA:
    li_repo_srv TYPE REF TO zif_abapgit_repo_srv,
    li_repo     TYPE REF TO zif_abapgit_repo,
    lv_reason   TYPE string.

  CASE abap_true.
    WHEN p_list.
      zcl_abapgit_persistence_user=>get_instance( )->set_repo_show( || ).
      MESSAGE 'Starting with Repository List' TYPE 'S'.
    WHEN p_last.
      " Use default behavior
      MESSAGE 'Starting with Last Repository' TYPE 'S'.
    WHEN p_repo.
      li_repo_srv = zcl_abapgit_repo_srv=>get_instance( ).
      li_repo_srv->get_repo_from_package(
        EXPORTING
          iv_package = p_pack
        IMPORTING
          ei_repo    = li_repo
          ev_reason  = lv_reason ).
      IF li_repo IS INITIAL.
        IF lv_reason IS INITIAL.
          MESSAGE 'No repository found that includes package' TYPE 'S'.
        ELSE.
          MESSAGE lv_reason TYPE 'S'.
        ENDIF.
        RETURN.
      ENDIF.
      zcl_abapgit_persistence_user=>get_instance( )->set_repo_show( li_repo->get_key( ) ).
  ENDCASE.

  CASE abap_true.
    WHEN p_devel.
      SUBMIT zabapgit.
    WHEN p_stand.
      SUBMIT zabapgit_standalone.
  ENDCASE.
