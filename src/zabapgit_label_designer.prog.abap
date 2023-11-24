REPORT zabapgit_label_designer.

********************************************************************************
* abapGit Label Designer
*
* A tool for designing labels for your personal abapGit settings
* Colors are based on the standard CSS colors supported by browsers
* https://www.w3schools.com/cssref/css_colors.php
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

CONSTANTS c_version TYPE string VALUE '1.0.0' ##NEEDED.

SELECTION-SCREEN BEGIN OF SCREEN 1001.
* dummy for triggering screen
SELECTION-SCREEN END OF SCREEN 1001.

DATA go_gui TYPE REF TO zcl_abapgit_label_designer.

AT SELECTION-SCREEN ON EXIT-COMMAND.

  CASE sy-ucomm.
    WHEN 'CBAC' OR 'CCAN'.  "Back & Escape
      IF go_gui->back( ) = abap_true.
        go_gui->free( ).
      ELSE.
        LEAVE TO SCREEN 1001.
      ENDIF.
  ENDCASE.

START-OF-SELECTION.

  CREATE OBJECT go_gui.

  go_gui->startup( '' )->render( ).

  CALL SELECTION-SCREEN 1001.
