       01  WK-DETALLE-PROVINCIA         PIC X(31).
       

       01  TAB-PROVINCIAS.
           03 FILLER                    PIC X(32)
              VALUE "CCiudad Autónoma de Buenos Aires".
           03 FILLER                    PIC X(32)
              VALUE "BBuenos Aires                   ".
           03 FILLER                    PIC X(32)
              VALUE "KCatamarca                      ".
           03 FILLER                    PIC X(32)
              VALUE "XCórdoba                        ".
           03 FILLER                    PIC X(32)
              VALUE "WCorrientes                     ".
           03 FILLER                    PIC X(32)
              VALUE "EEntre Ríos                     ".
           03 FILLER                    PIC X(32)
              VALUE "YJujuy                          ".
           03 FILLER                    PIC X(32)
              VALUE "MMendoza                        ".
           03 FILLER                    PIC X(32)
              VALUE "FLa Rioja                       ".
           03 FILLER                    PIC X(32)
              VALUE "ASalta                          ".
           03 FILLER                    PIC X(32)
              VALUE "JSan Juan                       ".
           03 FILLER                    PIC X(32)
              VALUE "DSan Luis                       ".
           03 FILLER                    PIC X(32)
              VALUE "SSanta Fe                       ".
           03 FILLER                    PIC X(32)
              VALUE "GSantiago del Estero            ".
           03 FILLER                    PIC X(32)
              VALUE "TTucumán                        ".
           03 FILLER                    PIC X(32)
              VALUE "HChaco                          ".
           03 FILLER                    PIC X(32)
              VALUE "UChubut                         ".
           03 FILLER                    PIC X(32)
              VALUE "PFormosa                        ".
           03 FILLER                    PIC X(32)
              VALUE "NMisiones                       ".
           03 FILLER                    PIC X(32)
              VALUE "QNeuquén                        ".
           03 FILLER                    PIC X(32)
              VALUE "LLa Pampa                       ".
           03 FILLER                    PIC X(32)
              VALUE "RRío Negro                      ".
           03 FILLER                    PIC X(32)
              VALUE "ZSanta Cruz                     ".
           03 FILLER                    PIC X(32)
              VALUE "VTierra del Fuego               ".
       01  TAB-PROVINCIAS-IDX REDEFINES TAB-PROVINCIAS-IDX.
           03 TAB-PROVINCIAS-DETALLE OCCURS 24 INDEXED BY PROV-INDEX.
              05 TAB-PROVINCIAS-COD     PIC X.
              05 TAB-PROVINCIAS-DETALLE PIC X(31).