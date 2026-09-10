  $ tramabol prog.cob --show
  { module_memory = <cob_module>; module_unit = <opaque>;
    module_data =
    { map = <opaque>;
      working_storage =
      { storage_records =
        [{ record_memory =
           { Types.record_data_ptr = <opaque>; record_data_size = 1 };
           record_definition = {
             record: TWO
             storage: WORKING-STORAGE
             item: {
               qualname: TWO
               offset: 0
               size: 8
               layout: {
                 elementary
                 usage: {
                   display
                   category: NUMERIC(digits = 1, scale = 0, signed = false)
                 }
                 value: 2
               }
             }
           } };
          { record_memory =
            { Types.record_data_ptr = <opaque>; record_data_size = 1 };
            record_definition = {
              record: ONE
              storage: WORKING-STORAGE
              item: {
                qualname: ONE
                offset: 0
                size: 8
                layout: {
                  elementary
                  usage: {
                    display
                    category: NUMERIC(digits = 1, scale = 0, signed = false)
                  }
                  value: 1
                }
              }
            } };
          { record_memory =
            { Types.record_data_ptr = <opaque>; record_data_size = 1 };
            record_definition = {
              record: ZER
              storage: WORKING-STORAGE
              item: {
                qualname: ZER
                offset: 0
                size: 8
                layout: {
                  elementary
                  usage: {
                    display
                    category: NUMERIC(digits = 1, scale = 0, signed = false)
                  }
                  value: 0
                }
              }
            } };
          { record_memory =
            { Types.record_data_ptr = <opaque>; record_data_size = 3 };
            record_definition = {
              record: Y
              storage: WORKING-STORAGE
              item: {
                qualname: Y
                offset: 0
                size: 24
                layout: {
                  elementary
                  usage: {
                    display
                    category: NUMERIC(digits = 3, scale = 0, signed = false)
                  }
                  value: 123
                }
              }
            } };
          { record_memory =
            { Types.record_data_ptr = <opaque>; record_data_size = 6 };
            record_definition = {
              record: X
              storage: WORKING-STORAGE
              item: {
                qualname: X
                offset: 0
                size: 48
                layout: {
                  elementary
                  usage: {
                    display
                    category: ALPHANUMERIC(6)
                  }
                  value: "ABCDEF"
                }
              }
            } }
          ];
        storage_fields =
        [(Direct_access
            { fixed_field = " ";
              fixed_field_info =
              { field_definition = {
                  qualname: TWO
                  offset: 0
                  size: 8
                  layout: {
                    elementary
                    usage: {
                      display
                      category: NUMERIC(digits = 1, scale = 0, signed = false)
                    }
                    value: 2
                  }
                }; field_initial_value = (Some 2/int32) }
              });
          (Direct_access
             { fixed_field = " ";
               fixed_field_info =
               { field_definition = {
                   qualname: ONE
                   offset: 0
                   size: 8
                   layout: {
                     elementary
                     usage: {
                       display
                       category: NUMERIC(digits = 1, scale = 0, signed = false)
                     }
                     value: 1
                   }
                 }; field_initial_value = (Some 1/int32) }
               });
          (Direct_access
             { fixed_field = " ";
               fixed_field_info =
               { field_definition = {
                   qualname: ZER
                   offset: 0
                   size: 8
                   layout: {
                     elementary
                     usage: {
                       display
                       category: NUMERIC(digits = 1, scale = 0, signed = false)
                     }
                     value: 0
                   }
                 }; field_initial_value = (Some 0/int32) }
               });
          (Direct_access
             { fixed_field = "   ";
               fixed_field_info =
               { field_definition = {
                   qualname: Y
                   offset: 0
                   size: 24
                   layout: {
                     elementary
                     usage: {
                       display
                       category: NUMERIC(digits = 3, scale = 0, signed = false)
                     }
                     value: 123
                   }
                 }; field_initial_value = (Some 123/int32) }
               });
          (Direct_access
             { fixed_field = "      ";
               fixed_field_info =
               { field_definition = {
                   qualname: X
                   offset: 0
                   size: 48
                   layout: {
                     elementary
                     usage: {
                       display
                       category: ALPHANUMERIC(6)
                     }
                     value: "ABCDEF"
                   }
                 }; field_initial_value = (Some "ABCDEF") }
               })
          ]
        };
      local_storage = { storage_records = []; storage_fields = [] } };
    module_proc =
    [IR_display {
       data_refs =
       [|{ data_field =
           Field_in_memory {field = (Fixed_field "      ");
             field_info =
             { field_definition = {
                 qualname: X
                 offset: 0
                 size: 48
                 layout: {
                   elementary
                   usage: {
                     display
                     category: ALPHANUMERIC(6)
                   }
                   value: "ABCDEF"
                 }
               }; field_initial_value = (Some "ABCDEF") }};
           data_ref_loc = <opaque>;
           data_refmod =
           (Some { refmod_left =
                   { field_ref = (Constant_field 1/int32);
                     field_ref_loc = <opaque> };
                   refmod_length =
                   (Some { field_ref = (Constant_field 2/int32);
                           field_ref_loc = <opaque> })
                   })
           }
         |];
       advancing = true};
      IR_display {
        data_refs =
        [|{ data_field =
            Field_in_memory {field = (Fixed_field "      ");
              field_info =
              { field_definition = {
                  qualname: X
                  offset: 0
                  size: 48
                  layout: {
                    elementary
                    usage: {
                      display
                      category: ALPHANUMERIC(6)
                    }
                    value: "ABCDEF"
                  }
                }; field_initial_value = (Some "ABCDEF") }};
            data_ref_loc = <opaque>;
            data_refmod =
            (Some { refmod_left =
                    { field_ref = (Constant_field 3/int32);
                      field_ref_loc = <opaque> };
                    refmod_length =
                    (Some { field_ref = (Constant_field 1/int32);
                            field_ref_loc = <opaque> })
                    })
            }
          |];
        advancing = true};
      IR_display {
        data_refs =
        [|{ data_field =
            Field_in_memory {field = (Fixed_field "      ");
              field_info =
              { field_definition = {
                  qualname: X
                  offset: 0
                  size: 48
                  layout: {
                    elementary
                    usage: {
                      display
                      category: ALPHANUMERIC(6)
                    }
                    value: "ABCDEF"
                  }
                }; field_initial_value = (Some "ABCDEF") }};
            data_ref_loc = <opaque>;
            data_refmod =
            (Some { refmod_left =
                    { field_ref = (Constant_field 4/int32);
                      field_ref_loc = <opaque> };
                    refmod_length =
                    (Some { field_ref = (Constant_field 3/int32);
                            field_ref_loc = <opaque> })
                    })
            }
          |];
        advancing = true};
      IR_display {
        data_refs =
        [|{ data_field =
            Field_in_memory {field = (Fixed_field "      ");
              field_info =
              { field_definition = {
                  qualname: X
                  offset: 0
                  size: 48
                  layout: {
                    elementary
                    usage: {
                      display
                      category: ALPHANUMERIC(6)
                    }
                    value: "ABCDEF"
                  }
                }; field_initial_value = (Some "ABCDEF") }};
            data_ref_loc = <opaque>;
            data_refmod =
            (Some { refmod_left =
                    { field_ref = (Constant_field 6/int32);
                      field_ref_loc = <opaque> };
                    refmod_length =
                    (Some { field_ref = (Constant_field 1/int32);
                            field_ref_loc = <opaque> })
                    })
            }
          |];
        advancing = true};
      IR_display {
        data_refs =
        [|{ data_field =
            Field_in_memory {field = (Fixed_field "      ");
              field_info =
              { field_definition = {
                  qualname: X
                  offset: 0
                  size: 48
                  layout: {
                    elementary
                    usage: {
                      display
                      category: ALPHANUMERIC(6)
                    }
                    value: "ABCDEF"
                  }
                }; field_initial_value = (Some "ABCDEF") }};
            data_ref_loc = <opaque>;
            data_refmod =
            (Some { refmod_left =
                    { field_ref = (Constant_field 6/int32);
                      field_ref_loc = <opaque> };
                    refmod_length =
                    (Some { field_ref =
                            Field_in_memory {field = (Fixed_field " ");
                              field_info =
                              { field_definition = {
                                  qualname: ONE
                                  offset: 0
                                  size: 8
                                  layout: {
                                    elementary
                                    usage: {
                                      display
                                      category:
                                       NUMERIC(digits = 1, scale = 0,
                                               signed = false)
                                    }
                                    value: 1
                                  }
                                }; field_initial_value = (Some 1/int32) }};
                            field_ref_loc = <opaque> })
                    })
            }
          |];
        advancing = true};
      IR_display {
        data_refs =
        [|{ data_field =
            Field_in_memory {field = (Fixed_field "      ");
              field_info =
              { field_definition = {
                  qualname: X
                  offset: 0
                  size: 48
                  layout: {
                    elementary
                    usage: {
                      display
                      category: ALPHANUMERIC(6)
                    }
                    value: "ABCDEF"
                  }
                }; field_initial_value = (Some "ABCDEF") }};
            data_ref_loc = <opaque>;
            data_refmod =
            (Some { refmod_left =
                    { field_ref = (Constant_field 1/int32);
                      field_ref_loc = <opaque> };
                    refmod_length = None })
            }
          |];
        advancing = true};
      IR_display {
        data_refs =
        [|{ data_field =
            Field_in_memory {field = (Fixed_field "      ");
              field_info =
              { field_definition = {
                  qualname: X
                  offset: 0
                  size: 48
                  layout: {
                    elementary
                    usage: {
                      display
                      category: ALPHANUMERIC(6)
                    }
                    value: "ABCDEF"
                  }
                }; field_initial_value = (Some "ABCDEF") }};
            data_ref_loc = <opaque>;
            data_refmod =
            (Some { refmod_left =
                    { field_ref = (Constant_field 3/int32);
                      field_ref_loc = <opaque> };
                    refmod_length = None })
            }
          |];
        advancing = true};
      IR_display {
        data_refs =
        [|{ data_field =
            Field_in_memory {field = (Fixed_field "      ");
              field_info =
              { field_definition = {
                  qualname: X
                  offset: 0
                  size: 48
                  layout: {
                    elementary
                    usage: {
                      display
                      category: ALPHANUMERIC(6)
                    }
                    value: "ABCDEF"
                  }
                }; field_initial_value = (Some "ABCDEF") }};
            data_ref_loc = <opaque>;
            data_refmod =
            (Some { refmod_left =
                    { field_ref = (Constant_field 4/int32);
                      field_ref_loc = <opaque> };
                    refmod_length = None })
            }
          |];
        advancing = true};
      IR_display {
        data_refs =
        [|{ data_field =
            Field_in_memory {field = (Fixed_field "      ");
              field_info =
              { field_definition = {
                  qualname: X
                  offset: 0
                  size: 48
                  layout: {
                    elementary
                    usage: {
                      display
                      category: ALPHANUMERIC(6)
                    }
                    value: "ABCDEF"
                  }
                }; field_initial_value = (Some "ABCDEF") }};
            data_ref_loc = <opaque>;
            data_refmod =
            (Some { refmod_left =
                    { field_ref = (Constant_field 6/int32);
                      field_ref_loc = <opaque> };
                    refmod_length = None })
            }
          |];
        advancing = true};
      IR_display {
        data_refs =
        [|{ data_field =
            Field_in_memory {field = (Fixed_field "      ");
              field_info =
              { field_definition = {
                  qualname: X
                  offset: 0
                  size: 48
                  layout: {
                    elementary
                    usage: {
                      display
                      category: ALPHANUMERIC(6)
                    }
                    value: "ABCDEF"
                  }
                }; field_initial_value = (Some "ABCDEF") }};
            data_ref_loc = <opaque>;
            data_refmod =
            (Some { refmod_left =
                    { field_ref =
                      Field_in_memory {field = (Fixed_field " ");
                        field_info =
                        { field_definition = {
                            qualname: ONE
                            offset: 0
                            size: 8
                            layout: {
                              elementary
                              usage: {
                                display
                                category:
                                 NUMERIC(digits = 1, scale = 0, signed = false)
                              }
                              value: 1
                            }
                          }; field_initial_value = (Some 1/int32) }};
                      field_ref_loc = <opaque> };
                    refmod_length = None })
            }
          |];
        advancing = true};
      IR_display {
        data_refs =
        [|{ data_field =
            Field_in_memory {field = (Fixed_field "   ");
              field_info =
              { field_definition = {
                  qualname: Y
                  offset: 0
                  size: 24
                  layout: {
                    elementary
                    usage: {
                      display
                      category: NUMERIC(digits = 3, scale = 0, signed = false)
                    }
                    value: 123
                  }
                }; field_initial_value = (Some 123/int32) }};
            data_ref_loc = <opaque>;
            data_refmod =
            (Some { refmod_left =
                    { field_ref = (Constant_field 1/int32);
                      field_ref_loc = <opaque> };
                    refmod_length =
                    (Some { field_ref = (Constant_field 3/int32);
                            field_ref_loc = <opaque> })
                    })
            }
          |];
        advancing = true};
      IR_display {
        data_refs =
        [|{ data_field =
            Field_in_memory {field = (Fixed_field "   ");
              field_info =
              { field_definition = {
                  qualname: Y
                  offset: 0
                  size: 24
                  layout: {
                    elementary
                    usage: {
                      display
                      category: NUMERIC(digits = 3, scale = 0, signed = false)
                    }
                    value: 123
                  }
                }; field_initial_value = (Some 123/int32) }};
            data_ref_loc = <opaque>;
            data_refmod =
            (Some { refmod_left =
                    { field_ref = (Constant_field 2/int32);
                      field_ref_loc = <opaque> };
                    refmod_length =
                    (Some { field_ref = (Constant_field 2/int32);
                            field_ref_loc = <opaque> })
                    })
            }
          |];
        advancing = true};
      IR_display {
        data_refs =
        [|{ data_field =
            Field_in_memory {field = (Fixed_field "   ");
              field_info =
              { field_definition = {
                  qualname: Y
                  offset: 0
                  size: 24
                  layout: {
                    elementary
                    usage: {
                      display
                      category: NUMERIC(digits = 3, scale = 0, signed = false)
                    }
                    value: 123
                  }
                }; field_initial_value = (Some 123/int32) }};
            data_ref_loc = <opaque>;
            data_refmod =
            (Some { refmod_left =
                    { field_ref = (Constant_field 3/int32);
                      field_ref_loc = <opaque> };
                    refmod_length =
                    (Some { field_ref = (Constant_field 1/int32);
                            field_ref_loc = <opaque> })
                    })
            }
          |];
        advancing = true};
      IR_display {
        data_refs =
        [|{ data_field =
            Field_in_memory {field = (Fixed_field "   ");
              field_info =
              { field_definition = {
                  qualname: Y
                  offset: 0
                  size: 24
                  layout: {
                    elementary
                    usage: {
                      display
                      category: NUMERIC(digits = 3, scale = 0, signed = false)
                    }
                    value: 123
                  }
                }; field_initial_value = (Some 123/int32) }};
            data_ref_loc = <opaque>;
            data_refmod =
            (Some { refmod_left =
                    { field_ref = (Constant_field 1/int32);
                      field_ref_loc = <opaque> };
                    refmod_length = None })
            }
          |];
        advancing = true};
      IR_display {
        data_refs =
        [|{ data_field =
            Field_in_memory {field = (Fixed_field "   ");
              field_info =
              { field_definition = {
                  qualname: Y
                  offset: 0
                  size: 24
                  layout: {
                    elementary
                    usage: {
                      display
                      category: NUMERIC(digits = 3, scale = 0, signed = false)
                    }
                    value: 123
                  }
                }; field_initial_value = (Some 123/int32) }};
            data_ref_loc = <opaque>;
            data_refmod =
            (Some { refmod_left =
                    { field_ref = (Constant_field 2/int32);
                      field_ref_loc = <opaque> };
                    refmod_length = None })
            }
          |];
        advancing = true};
      IR_display {
        data_refs =
        [|{ data_field =
            Field_in_memory {field = (Fixed_field "   ");
              field_info =
              { field_definition = {
                  qualname: Y
                  offset: 0
                  size: 24
                  layout: {
                    elementary
                    usage: {
                      display
                      category: NUMERIC(digits = 3, scale = 0, signed = false)
                    }
                    value: 123
                  }
                }; field_initial_value = (Some 123/int32) }};
            data_ref_loc = <opaque>;
            data_refmod =
            (Some { refmod_left =
                    { field_ref = (Constant_field 3/int32);
                      field_ref_loc = <opaque> };
                    refmod_length = None })
            }
          |];
        advancing = true};
      IR_stop {optional_status = None}]
    }

  $ tramabol prog.cob
  AB
  C
  DEF
  F
  F
  ABCDEF
  CDEF
  DEF
  F
  ABCDEF
  123
  23
  3
  123
  23
  3
  Terminated with status: 0

  $ ERROR=0 tramabol prog.cob
  AB
  C
  DEF
  F
  F
  ABCDEF
  CDEF
  DEF
  F
  ABCDEF
  123
  23
  3
  123
  23
  3
  prog.cob:28.24-28.25:
    25              display Y (2:)
    26              display Y (3:)
    27          >>IF   ERROR = 0
    28 >            display X (4:4)
  ----                           ^
    29          >>ELIF ERROR = 1
    30              display X (6:2)
  Error: Invalid length in reference modification: got 4, expected in
         [1..3] when given offset is 4
  [1]

  $ ERROR=1 tramabol prog.cob
  AB
  C
  DEF
  F
  F
  ABCDEF
  CDEF
  DEF
  F
  ABCDEF
  123
  23
  3
  123
  23
  3
  prog.cob:30.24-30.25:
    27          >>IF   ERROR = 0
    28              display X (4:4)
    29          >>ELIF ERROR = 1
    30 >            display X (6:2)
  ----                           ^
    31          >>ELIF ERROR = 2
    32              display X (6:TWO)
  Error: Invalid length in reference modification: got 2, expected in
         [1..1] when given offset is 6
  [1]

  $ ERROR=2 tramabol prog.cob
  AB
  C
  DEF
  F
  F
  ABCDEF
  CDEF
  DEF
  F
  ABCDEF
  123
  23
  3
  123
  23
  3
  prog.cob:32.24-32.27:
    29          >>ELIF ERROR = 1
    30              display X (6:2)
    31          >>ELIF ERROR = 2
    32 >            display X (6:TWO)
  ----                           ^^^
    33          >>ELIF ERROR = 3
    34              display X (-1:1)
  Error: Invalid length in reference modification: got 2, expected in
         [1..1] when given offset is 6
  [1]

  $ ERROR=3 tramabol prog.cob
  AB
  C
  DEF
  F
  F
  ABCDEF
  CDEF
  DEF
  F
  ABCDEF
  123
  23
  3
  123
  23
  3
  prog.cob:34.22-34.24:
    31          >>ELIF ERROR = 2
    32              display X (6:TWO)
    33          >>ELIF ERROR = 3
    34 >            display X (-1:1)
  ----                         ^^
    35          >>ELIF ERROR = 4
    36              display Y (4:1)
  Error: Invalid offset in reference modification: got -1, expected in [1..7]
  [1]

  $ ERROR=4 tramabol prog.cob
  AB
  C
  DEF
  F
  F
  ABCDEF
  CDEF
  DEF
  F
  ABCDEF
  123
  23
  3
  123
  23
  3
  prog.cob:36.24-36.25:
    33          >>ELIF ERROR = 3
    34              display X (-1:1)
    35          >>ELIF ERROR = 4
    36 >            display Y (4:1)
  ----                           ^
    37          >>ELIF ERROR = 5
    38              display Y (0:1)
  Error: Invalid length in reference modification: got 1, expected in
         [1..0] when given offset is 4
  [1]

  $ ERROR=5 tramabol prog.cob
  AB
  C
  DEF
  F
  F
  ABCDEF
  CDEF
  DEF
  F
  ABCDEF
  123
  23
  3
  123
  23
  3
  prog.cob:38.22-38.23:
    35          >>ELIF ERROR = 4
    36              display Y (4:1)
    37          >>ELIF ERROR = 5
    38 >            display Y (0:1)
  ----                         ^
    39          >>ELIF ERROR = 6
    40              display Y (ZER:1)
  Error: Invalid offset in reference modification: got 0, expected in [1..4]
  [1]

  $ ERROR=6 tramabol prog.cob
  AB
  C
  DEF
  F
  F
  ABCDEF
  CDEF
  DEF
  F
  ABCDEF
  123
  23
  3
  123
  23
  3
  prog.cob:40.22-40.25:
    37          >>ELIF ERROR = 5
    38              display Y (0:1)
    39          >>ELIF ERROR = 6
    40 >            display Y (ZER:1)
  ----                         ^^^
    41          >>END-IF
    42              stop run.
  Error: Invalid offset in reference modification: got 0, expected in [1..4]
  [1]
