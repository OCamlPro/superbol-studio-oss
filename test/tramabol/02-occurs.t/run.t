  $ tramabol ok-1.cob --show
  { module_memory = <cob_module>; module_unit = <opaque>;
    module_data =
    { map = <opaque>;
      working_storage =
      { storage_records =
        [{ record_memory =
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
               redefs: {
                 table
                 redefines: X
                 offset: 0
                 size: 48
                 range: {
                   span: fixed-length: 6
                 }
                 field: {
                   qualname: Y
                   leading ranges: 1
                   offset: 0
                   size: 8
                   layout: {
                     elementary
                     usage: {
                       display
                       category: ALPHANUMERIC(1)
                     }
                   }
                 }
               }{
                 table
                 redefines: X
                 offset: 0
                 size: 48
                 range: {
                   span: fixed-length: 3
                 }
                 field: {
                   qualname: Z
                   leading ranges: 1
                   offset: 0
                   size: 16
                   layout: {
                     elementary
                     usage: {
                       display
                       category: ALPHANUMERIC(2)
                     }
                   }
                 }
               }
             }
           } }
          ];
        storage_fields =
        [Indirect_access {
           base_field =
           { fixed_field = "  ";
             fixed_field_info =
             { field_definition = {
                 qualname: Z
                 leading ranges: 1
                 offset: 0
                 size: 16
                 layout: {
                   elementary
                   usage: {
                     display
                     category: ALPHANUMERIC(2)
                   }
                 }
               }; field_initial_value = None }
             };
           ranges = [Fixed_range {max = 3}]};
          Indirect_access {
            base_field =
            { fixed_field = " ";
              fixed_field_info =
              { field_definition = {
                  qualname: Y
                  leading ranges: 1
                  offset: 0
                  size: 8
                  layout: {
                    elementary
                    usage: {
                      display
                      category: ALPHANUMERIC(1)
                    }
                  }
                }; field_initial_value = None }
              };
            ranges = [Fixed_range {max = 6}]};
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
                   redefs: {
                     table
                     redefines: X
                     offset: 0
                     size: 48
                     range: {
                       span: fixed-length: 6
                     }
                     field: {
                       qualname: Y
                       leading ranges: 1
                       offset: 0
                       size: 8
                       layout: {
                         elementary
                         usage: {
                           display
                           category: ALPHANUMERIC(1)
                         }
                       }
                     }
                   }{
                     table
                     redefines: X
                     offset: 0
                     size: 48
                     range: {
                       span: fixed-length: 3
                     }
                     field: {
                       qualname: Z
                       leading ranges: 1
                       offset: 0
                       size: 16
                       layout: {
                         elementary
                         usage: {
                           display
                           category: ALPHANUMERIC(2)
                         }
                       }
                     }
                   }
                 }; field_initial_value = (Some "ABCDEF") }
               })
          ]
        };
      local_storage = { storage_records = []; storage_fields = [] } };
    module_proc =
    [IR_display {
       data_refs =
       [|{ field_ref =
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
                 redefs: {
                   table
                   redefines: X
                   offset: 0
                   size: 48
                   range: {
                     span: fixed-length: 6
                   }
                   field: {
                     qualname: Y
                     leading ranges: 1
                     offset: 0
                     size: 8
                     layout: {
                       elementary
                       usage: {
                         display
                         category: ALPHANUMERIC(1)
                       }
                     }
                   }
                 }{
                   table
                   redefines: X
                   offset: 0
                   size: 48
                   range: {
                     span: fixed-length: 3
                   }
                   field: {
                     qualname: Z
                     leading ranges: 1
                     offset: 0
                     size: 16
                     layout: {
                       elementary
                       usage: {
                         display
                         category: ALPHANUMERIC(2)
                       }
                     }
                   }
                 }
               }; field_initial_value = (Some "ABCDEF") }};
           field_ref_loc = <opaque> }
         |];
       advancing = true};
      IR_display {
        data_refs =
        [|{ field_ref =
            Field_in_memory {
              field =
              (Table_field
                 { cell_first_field = (Fixed_field " ");
                   cell_index_field =
                   { field_ref = (Constant_field 1/int32);
                     field_ref_loc = <opaque> };
                   cell_index_max = 6; cell_stride = 1 });
              field_info =
              { field_definition = {
                  qualname: Y
                  leading ranges: 1
                  offset: 0
                  size: 8
                  layout: {
                    elementary
                    usage: {
                      display
                      category: ALPHANUMERIC(1)
                    }
                  }
                }; field_initial_value = None }};
            field_ref_loc = <opaque> };
          { field_ref = (Constant_field "*"); field_ref_loc = <opaque> };
          { field_ref =
            Field_in_memory {
              field =
              (Table_field
                 { cell_first_field = (Fixed_field " ");
                   cell_index_field =
                   { field_ref = (Constant_field 6/int32);
                     field_ref_loc = <opaque> };
                   cell_index_max = 6; cell_stride = 1 });
              field_info =
              { field_definition = {
                  qualname: Y
                  leading ranges: 1
                  offset: 0
                  size: 8
                  layout: {
                    elementary
                    usage: {
                      display
                      category: ALPHANUMERIC(1)
                    }
                  }
                }; field_initial_value = None }};
            field_ref_loc = <opaque> }
          |];
        advancing = true};
      IR_display {
        data_refs =
        [|{ field_ref =
            Field_in_memory {
              field =
              (Table_field
                 { cell_first_field = (Fixed_field "  ");
                   cell_index_field =
                   { field_ref = (Constant_field 1/int32);
                     field_ref_loc = <opaque> };
                   cell_index_max = 3; cell_stride = 1 });
              field_info =
              { field_definition = {
                  qualname: Z
                  leading ranges: 1
                  offset: 0
                  size: 16
                  layout: {
                    elementary
                    usage: {
                      display
                      category: ALPHANUMERIC(2)
                    }
                  }
                }; field_initial_value = None }};
            field_ref_loc = <opaque> };
          { field_ref = (Constant_field "*"); field_ref_loc = <opaque> };
          { field_ref =
            Field_in_memory {
              field =
              (Table_field
                 { cell_first_field = (Fixed_field "  ");
                   cell_index_field =
                   { field_ref = (Constant_field 2/int32);
                     field_ref_loc = <opaque> };
                   cell_index_max = 3; cell_stride = 1 });
              field_info =
              { field_definition = {
                  qualname: Z
                  leading ranges: 1
                  offset: 0
                  size: 16
                  layout: {
                    elementary
                    usage: {
                      display
                      category: ALPHANUMERIC(2)
                    }
                  }
                }; field_initial_value = None }};
            field_ref_loc = <opaque> };
          { field_ref = (Constant_field "*"); field_ref_loc = <opaque> };
          { field_ref =
            Field_in_memory {
              field =
              (Table_field
                 { cell_first_field = (Fixed_field "  ");
                   cell_index_field =
                   { field_ref = (Constant_field 3/int32);
                     field_ref_loc = <opaque> };
                   cell_index_max = 3; cell_stride = 1 });
              field_info =
              { field_definition = {
                  qualname: Z
                  leading ranges: 1
                  offset: 0
                  size: 16
                  layout: {
                    elementary
                    usage: {
                      display
                      category: ALPHANUMERIC(2)
                    }
                  }
                }; field_initial_value = None }};
            field_ref_loc = <opaque> }
          |];
        advancing = true};
      IR_stop {optional_status = None}]
    }

  $ tramabol ok-1.cob
  ABCDEF
  A*F
  AB*CD*EF
  Terminated with status: 0

  $ tramabol ok-2.cob --show
  { module_memory = <cob_module>; module_unit = <opaque>;
    module_data =
    { map = <opaque>;
      working_storage =
      { storage_records =
        [{ record_memory =
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
               redefs: {
                 table
                 redefines: X
                 offset: 0
                 size: 48
                 range: {
                   span: fixed-length: 2
                 }
                 field: {
                   qualname: Y-1
                   leading ranges: 1
                   offset: 0
                   size: 24
                   layout: {
                     structure
                     fields: {
                       table
                       offset: 0
                       size: 24
                       range: {
                         span: fixed-length: 3
                       }
                       field: {
                         qualname: Y-2 IN Y-1
                         leading ranges: 2
                         offset: 0
                         size: 8
                         layout: {
                           elementary
                           usage: {
                             display
                             category: ALPHANUMERIC(1)
                           }
                         }
                       }
                     }
                   }
                 }
               }{
                 table
                 redefines: X
                 offset: 0
                 size: 48
                 range: {
                   span: fixed-length: 3
                 }
                 field: {
                   qualname: Z-1
                   leading ranges: 1
                   offset: 0
                   size: 16
                   layout: {
                     elementary
                     usage: {
                       display
                       category: ALPHANUMERIC(2)
                     }
                   }
                 }
               }{
                 table
                 redefines: X
                 offset: 0
                 size: 48
                 range: {
                   span: fixed-length: 1
                 }
                 field: {
                   qualname: T-1
                   leading ranges: 1
                   offset: 0
                   size: 48
                   layout: {
                     structure
                     fields: {
                       table
                       offset: 0
                       size: 48
                       range: {
                         span: fixed-length: 1
                       }
                       field: {
                         qualname: T-2 IN T-1
                         leading ranges: 2
                         offset: 0
                         size: 48
                         layout: {
                           structure
                           fields: {
                             table
                             offset: 0
                             size: 48
                             range: {
                               span: fixed-length: 3
                             }
                             field: {
                               qualname: T-3 IN T-2 IN T-1
                               leading ranges: 3
                               offset: 0
                               size: 16
                               layout: {
                                 elementary
                                 usage: {
                                   display
                                   category: ALPHANUMERIC(2)
                                 }
                               }
                             }
                           }
                         }
                       }
                     }
                   }
                 }
               }
             }
           } }
          ];
        storage_fields =
        [Indirect_access {
           base_field =
           { fixed_field = "  ";
             fixed_field_info =
             { field_definition = {
                 qualname: T-3 IN T-2 IN T-1
                 leading ranges: 3
                 offset: 0
                 size: 16
                 layout: {
                   elementary
                   usage: {
                     display
                     category: ALPHANUMERIC(2)
                   }
                 }
               }; field_initial_value = None }
             };
           ranges =
           [Fixed_range {max = 1}, Fixed_range {max = 1}, Fixed_range {max = 3}]};
          Indirect_access {
            base_field =
            { fixed_field = "      ";
              fixed_field_info =
              { field_definition = {
                  qualname: T-2 IN T-1
                  leading ranges: 2
                  offset: 0
                  size: 48
                  layout: {
                    structure
                    fields: {
                      table
                      offset: 0
                      size: 48
                      range: {
                        span: fixed-length: 3
                      }
                      field: {
                        qualname: T-3 IN T-2 IN T-1
                        leading ranges: 3
                        offset: 0
                        size: 16
                        layout: {
                          elementary
                          usage: {
                            display
                            category: ALPHANUMERIC(2)
                          }
                        }
                      }
                    }
                  }
                }; field_initial_value = None }
              };
            ranges = [Fixed_range {max = 1}, Fixed_range {max = 1}]};
          Indirect_access {
            base_field =
            { fixed_field = "      ";
              fixed_field_info =
              { field_definition = {
                  qualname: T-1
                  leading ranges: 1
                  offset: 0
                  size: 48
                  layout: {
                    structure
                    fields: {
                      table
                      offset: 0
                      size: 48
                      range: {
                        span: fixed-length: 1
                      }
                      field: {
                        qualname: T-2 IN T-1
                        leading ranges: 2
                        offset: 0
                        size: 48
                        layout: {
                          structure
                          fields: {
                            table
                            offset: 0
                            size: 48
                            range: {
                              span: fixed-length: 3
                            }
                            field: {
                              qualname: T-3 IN T-2 IN T-1
                              leading ranges: 3
                              offset: 0
                              size: 16
                              layout: {
                                elementary
                                usage: {
                                  display
                                  category: ALPHANUMERIC(2)
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }; field_initial_value = None }
              };
            ranges = [Fixed_range {max = 1}]};
          Indirect_access {
            base_field =
            { fixed_field = "  ";
              fixed_field_info =
              { field_definition = {
                  qualname: Z-1
                  leading ranges: 1
                  offset: 0
                  size: 16
                  layout: {
                    elementary
                    usage: {
                      display
                      category: ALPHANUMERIC(2)
                    }
                  }
                }; field_initial_value = None }
              };
            ranges = [Fixed_range {max = 3}]};
          Indirect_access {
            base_field =
            { fixed_field = " ";
              fixed_field_info =
              { field_definition = {
                  qualname: Y-2 IN Y-1
                  leading ranges: 2
                  offset: 0
                  size: 8
                  layout: {
                    elementary
                    usage: {
                      display
                      category: ALPHANUMERIC(1)
                    }
                  }
                }; field_initial_value = None }
              };
            ranges = [Fixed_range {max = 2}, Fixed_range {max = 3}]};
          Indirect_access {
            base_field =
            { fixed_field = "   ";
              fixed_field_info =
              { field_definition = {
                  qualname: Y-1
                  leading ranges: 1
                  offset: 0
                  size: 24
                  layout: {
                    structure
                    fields: {
                      table
                      offset: 0
                      size: 24
                      range: {
                        span: fixed-length: 3
                      }
                      field: {
                        qualname: Y-2 IN Y-1
                        leading ranges: 2
                        offset: 0
                        size: 8
                        layout: {
                          elementary
                          usage: {
                            display
                            category: ALPHANUMERIC(1)
                          }
                        }
                      }
                    }
                  }
                }; field_initial_value = None }
              };
            ranges = [Fixed_range {max = 2}]};
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
                   redefs: {
                     table
                     redefines: X
                     offset: 0
                     size: 48
                     range: {
                       span: fixed-length: 2
                     }
                     field: {
                       qualname: Y-1
                       leading ranges: 1
                       offset: 0
                       size: 24
                       layout: {
                         structure
                         fields: {
                           table
                           offset: 0
                           size: 24
                           range: {
                             span: fixed-length: 3
                           }
                           field: {
                             qualname: Y-2 IN Y-1
                             leading ranges: 2
                             offset: 0
                             size: 8
                             layout: {
                               elementary
                               usage: {
                                 display
                                 category: ALPHANUMERIC(1)
                               }
                             }
                           }
                         }
                       }
                     }
                   }{
                     table
                     redefines: X
                     offset: 0
                     size: 48
                     range: {
                       span: fixed-length: 3
                     }
                     field: {
                       qualname: Z-1
                       leading ranges: 1
                       offset: 0
                       size: 16
                       layout: {
                         elementary
                         usage: {
                           display
                           category: ALPHANUMERIC(2)
                         }
                       }
                     }
                   }{
                     table
                     redefines: X
                     offset: 0
                     size: 48
                     range: {
                       span: fixed-length: 1
                     }
                     field: {
                       qualname: T-1
                       leading ranges: 1
                       offset: 0
                       size: 48
                       layout: {
                         structure
                         fields: {
                           table
                           offset: 0
                           size: 48
                           range: {
                             span: fixed-length: 1
                           }
                           field: {
                             qualname: T-2 IN T-1
                             leading ranges: 2
                             offset: 0
                             size: 48
                             layout: {
                               structure
                               fields: {
                                 table
                                 offset: 0
                                 size: 48
                                 range: {
                                   span: fixed-length: 3
                                 }
                                 field: {
                                   qualname: T-3 IN T-2 IN T-1
                                   leading ranges: 3
                                   offset: 0
                                   size: 16
                                   layout: {
                                     elementary
                                     usage: {
                                       display
                                       category: ALPHANUMERIC(2)
                                     }
                                   }
                                 }
                               }
                             }
                           }
                         }
                       }
                     }
                   }
                 }; field_initial_value = (Some "ABCDEF") }
               })
          ]
        };
      local_storage = { storage_records = []; storage_fields = [] } };
    module_proc =
    [IR_display {
       data_refs =
       [|{ field_ref =
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
                 redefs: {
                   table
                   redefines: X
                   offset: 0
                   size: 48
                   range: {
                     span: fixed-length: 2
                   }
                   field: {
                     qualname: Y-1
                     leading ranges: 1
                     offset: 0
                     size: 24
                     layout: {
                       structure
                       fields: {
                         table
                         offset: 0
                         size: 24
                         range: {
                           span: fixed-length: 3
                         }
                         field: {
                           qualname: Y-2 IN Y-1
                           leading ranges: 2
                           offset: 0
                           size: 8
                           layout: {
                             elementary
                             usage: {
                               display
                               category: ALPHANUMERIC(1)
                             }
                           }
                         }
                       }
                     }
                   }
                 }{
                   table
                   redefines: X
                   offset: 0
                   size: 48
                   range: {
                     span: fixed-length: 3
                   }
                   field: {
                     qualname: Z-1
                     leading ranges: 1
                     offset: 0
                     size: 16
                     layout: {
                       elementary
                       usage: {
                         display
                         category: ALPHANUMERIC(2)
                       }
                     }
                   }
                 }{
                   table
                   redefines: X
                   offset: 0
                   size: 48
                   range: {
                     span: fixed-length: 1
                   }
                   field: {
                     qualname: T-1
                     leading ranges: 1
                     offset: 0
                     size: 48
                     layout: {
                       structure
                       fields: {
                         table
                         offset: 0
                         size: 48
                         range: {
                           span: fixed-length: 1
                         }
                         field: {
                           qualname: T-2 IN T-1
                           leading ranges: 2
                           offset: 0
                           size: 48
                           layout: {
                             structure
                             fields: {
                               table
                               offset: 0
                               size: 48
                               range: {
                                 span: fixed-length: 3
                               }
                               field: {
                                 qualname: T-3 IN T-2 IN T-1
                                 leading ranges: 3
                                 offset: 0
                                 size: 16
                                 layout: {
                                   elementary
                                   usage: {
                                     display
                                     category: ALPHANUMERIC(2)
                                   }
                                 }
                               }
                             }
                           }
                         }
                       }
                     }
                   }
                 }
               }; field_initial_value = (Some "ABCDEF") }};
           field_ref_loc = <opaque> }
         |];
       advancing = true};
      IR_display {
        data_refs =
        [|{ field_ref =
            Field_in_memory {
              field =
              (Table_field
                 { cell_first_field = (Fixed_field "   ");
                   cell_index_field =
                   { field_ref = (Constant_field 1/int32);
                     field_ref_loc = <opaque> };
                   cell_index_max = 2; cell_stride = 1 });
              field_info =
              { field_definition = {
                  qualname: Y-1
                  leading ranges: 1
                  offset: 0
                  size: 24
                  layout: {
                    structure
                    fields: {
                      table
                      offset: 0
                      size: 24
                      range: {
                        span: fixed-length: 3
                      }
                      field: {
                        qualname: Y-2 IN Y-1
                        leading ranges: 2
                        offset: 0
                        size: 8
                        layout: {
                          elementary
                          usage: {
                            display
                            category: ALPHANUMERIC(1)
                          }
                        }
                      }
                    }
                  }
                }; field_initial_value = None }};
            field_ref_loc = <opaque> }
          |];
        advancing = true};
      IR_display {
        data_refs =
        [|{ field_ref =
            Field_in_memory {
              field =
              (Table_field
                 { cell_first_field =
                   (Table_field
                      { cell_first_field = (Fixed_field " ");
                        cell_index_field =
                        { field_ref = (Constant_field 3/int32);
                          field_ref_loc = <opaque> };
                        cell_index_max = 3; cell_stride = 1 });
                   cell_index_field =
                   { field_ref = (Constant_field 1/int32);
                     field_ref_loc = <opaque> };
                   cell_index_max = 2; cell_stride = 3 });
              field_info =
              { field_definition = {
                  qualname: Y-2 IN Y-1
                  leading ranges: 2
                  offset: 0
                  size: 8
                  layout: {
                    elementary
                    usage: {
                      display
                      category: ALPHANUMERIC(1)
                    }
                  }
                }; field_initial_value = None }};
            field_ref_loc = <opaque> }
          |];
        advancing = true};
      IR_display {
        data_refs =
        [|{ field_ref =
            Field_in_memory {
              field =
              (Table_field
                 { cell_first_field =
                   (Table_field
                      { cell_first_field = (Fixed_field " ");
                        cell_index_field =
                        { field_ref = (Constant_field 3/int32);
                          field_ref_loc = <opaque> };
                        cell_index_max = 3; cell_stride = 1 });
                   cell_index_field =
                   { field_ref = (Constant_field 2/int32);
                     field_ref_loc = <opaque> };
                   cell_index_max = 2; cell_stride = 3 });
              field_info =
              { field_definition = {
                  qualname: Y-2 IN Y-1
                  leading ranges: 2
                  offset: 0
                  size: 8
                  layout: {
                    elementary
                    usage: {
                      display
                      category: ALPHANUMERIC(1)
                    }
                  }
                }; field_initial_value = None }};
            field_ref_loc = <opaque> }
          |];
        advancing = true};
      IR_display {
        data_refs =
        [|{ field_ref =
            Field_in_memory {
              field =
              (Table_field
                 { cell_first_field = (Fixed_field "  ");
                   cell_index_field =
                   { field_ref = (Constant_field 1/int32);
                     field_ref_loc = <opaque> };
                   cell_index_max = 3; cell_stride = 1 });
              field_info =
              { field_definition = {
                  qualname: Z-1
                  leading ranges: 1
                  offset: 0
                  size: 16
                  layout: {
                    elementary
                    usage: {
                      display
                      category: ALPHANUMERIC(2)
                    }
                  }
                }; field_initial_value = None }};
            field_ref_loc = <opaque> }
          |];
        advancing = true};
      IR_display {
        data_refs =
        [|{ field_ref =
            Field_in_memory {
              field =
              (Table_field
                 { cell_first_field = (Fixed_field "  ");
                   cell_index_field =
                   { field_ref = (Constant_field 3/int32);
                     field_ref_loc = <opaque> };
                   cell_index_max = 3; cell_stride = 1 });
              field_info =
              { field_definition = {
                  qualname: Z-1
                  leading ranges: 1
                  offset: 0
                  size: 16
                  layout: {
                    elementary
                    usage: {
                      display
                      category: ALPHANUMERIC(2)
                    }
                  }
                }; field_initial_value = None }};
            field_ref_loc = <opaque> }
          |];
        advancing = true};
      IR_display {
        data_refs =
        [|{ field_ref =
            Field_in_memory {
              field =
              (Table_field
                 { cell_first_field =
                   (Table_field
                      { cell_first_field =
                        (Table_field
                           { cell_first_field = (Fixed_field "  ");
                             cell_index_field =
                             { field_ref = (Constant_field 2/int32);
                               field_ref_loc = <opaque> };
                             cell_index_max = 3; cell_stride = 1 });
                        cell_index_field =
                        { field_ref = (Constant_field 1/int32);
                          field_ref_loc = <opaque> };
                        cell_index_max = 1; cell_stride = 3 });
                   cell_index_field =
                   { field_ref = (Constant_field 1/int32);
                     field_ref_loc = <opaque> };
                   cell_index_max = 1; cell_stride = 3 });
              field_info =
              { field_definition = {
                  qualname: T-3 IN T-2 IN T-1
                  leading ranges: 3
                  offset: 0
                  size: 16
                  layout: {
                    elementary
                    usage: {
                      display
                      category: ALPHANUMERIC(2)
                    }
                  }
                }; field_initial_value = None }};
            field_ref_loc = <opaque> }
          |];
        advancing = true};
      IR_stop {optional_status = None}]
    }

  $ tramabol ok-2.cob
  ABCDEF
  ABC
  C
  F
  AB
  EF
  CD
  Terminated with status: 0

  $ CHECK=0 tramabol ko-1.cob
  ko-1.cob:10.22-10.23:
     7          01 Z REDEFINES X PIC XX   OCCURS 3.
     8          procedure division.
     9          >>IF   CHECK = 0
    10 >            display Y (0)
  ----                         ^
    11          >>ELIF CHECK = 1
    12              display Y (7)
  Error: Index is out of bounds: got 0, expected in [1..6]
  [1]
  $ CHECK=1 tramabol ko-1.cob
  ko-1.cob:12.22-12.23:
     9          >>IF   CHECK = 0
    10              display Y (0)
    11          >>ELIF CHECK = 1
    12 >            display Y (7)
  ----                         ^
    13          >>ELIF CHECK = 2
    14              display Z (-1)
  Error: Index is out of bounds: got 7, expected in [1..6]
  [1]
  $ CHECK=2 tramabol ko-1.cob
  ko-1.cob:14.22-14.24:
    11          >>ELIF CHECK = 1
    12              display Y (7)
    13          >>ELIF CHECK = 2
    14 >            display Z (-1)
  ----                         ^^
    15          >>ELIF CHECK = 3
    16              display Z (4)
  Error: Index is out of bounds: got -1, expected in [1..3]
  [1]
  $ CHECK=3 tramabol ko-1.cob
  ko-1.cob:16.22-16.23:
    13          >>ELIF CHECK = 2
    14              display Z (-1)
    15          >>ELIF CHECK = 3
    16 >            display Z (4)
  ----                         ^
    17          >>ELSE
    18              display Z ("a")
  Error: Index is out of bounds: got 4, expected in [1..3]
  [1]
  $ CHECK=4 tramabol ko-1.cob
  ko-1.cob:18.22-18.25:
    15          >>ELIF CHECK = 3
    16              display Z (4)
    17          >>ELSE
    18 >            display Z ("a")
  ----                         ^^^
    19          >>END-IF
    20              stop run.
  Error: Invalid data-type encountered (integer expected)
  [1]

  $ CHECK=0 tramabol ko-2.cob
  ko-2.cob:14.24-14.25:
    11                 03 T-3      OCCURS 3 PIC XX.
    12          procedure division.
    13          >>IF   CHECK = 0
    14 >            display Y-2 (0, 3)
  ----                           ^
    15          >>ELIF CHECK = 1
    16              display Y-2 (3, 3)
  Error: Index is out of bounds: got 0, expected in [1..2]
  [1]
  $ CHECK=1 tramabol ko-2.cob
  ko-2.cob:16.24-16.25:
    13          >>IF   CHECK = 0
    14              display Y-2 (0, 3)
    15          >>ELIF CHECK = 1
    16 >            display Y-2 (3, 3)
  ----                           ^
    17          >>ELIF CHECK = 2
    18              display Y-2 (1, 4)
  Error: Index is out of bounds: got 3, expected in [1..2]
  [1]
  $ CHECK=2 tramabol ko-2.cob
  ko-2.cob:18.27-18.28:
    15          >>ELIF CHECK = 1
    16              display Y-2 (3, 3)
    17          >>ELIF CHECK = 2
    18 >            display Y-2 (1, 4)
  ----                              ^
    19          >>ELIF CHECK = 3
    20              display Y-2 (1, -1)
  Error: Index is out of bounds: got 4, expected in [1..3]
  [1]
  $ CHECK=3 tramabol ko-2.cob
  ko-2.cob:20.27-20.29:
    17          >>ELIF CHECK = 2
    18              display Y-2 (1, 4)
    19          >>ELIF CHECK = 3
    20 >            display Y-2 (1, -1)
  ----                              ^^
    21          >>ELIF CHECK = 4
    22              display Y-2 (1)
  Error: Index is out of bounds: got -1, expected in [1..3]
  [1]
  $ CHECK=4 tramabol ko-2.cob
  ko-2.cob:22.19-22.22:
    19          >>ELIF CHECK = 3
    20              display Y-2 (1, -1)
    21          >>ELIF CHECK = 4
    22 >            display Y-2 (1)
  ----                      ^^^
    23          >>ELIF CHECK = 5
    24              display T-3 (1, 2)
  Error: missing 1 subscript for data-name 'Y-2'
  [1]
  $ CHECK=5 tramabol ko-2.cob
  ko-2.cob:24.19-24.22:
    21          >>ELIF CHECK = 4
    22              display Y-2 (1)
    23          >>ELIF CHECK = 5
    24 >            display T-3 (1, 2)
  ----                      ^^^
    25          >>ELIF CHECK = 6
    26              display T-3 (1, 2, 3, 4)
  Error: missing 1 subscript for data-name 'T-3'
  [1]
  $ CHECK=6 tramabol ko-2.cob
  ko-2.cob:26.33-26.34:
    23          >>ELIF CHECK = 5
    24              display T-3 (1, 2)
    25          >>ELIF CHECK = 6
    26 >            display T-3 (1, 2, 3, 4)
  ----                                    ^
    27          >>ELIF CHECK = 7
    28              display T-3 (1, 2, 0.5, 4)
  Error: extraneous subscript for data-name 'T-3'
  [1]
  $ CHECK=7 tramabol ko-2.cob
  ko-2.cob:28.35-28.36:
    25          >>ELIF CHECK = 6
    26              display T-3 (1, 2, 3, 4)
    27          >>ELIF CHECK = 7
    28 >            display T-3 (1, 2, 0.5, 4)
  ----                                      ^
    29          >>ELIF CHECK = 8
    30              display T-3 (1, 2, 0.5)
  Error: extraneous subscript for data-name 'T-3'
  [1]
TODO: For now an unsupported literal; should be about an invalid type
for the index instead.
  $ CHECK=8 tramabol ko-2.cob
  ko-2.cob:30.30-30.33:
    27          >>ELIF CHECK = 7
    28              display T-3 (1, 2, 0.5, 4)
    29          >>ELIF CHECK = 8
    30 >            display T-3 (1, 2, 0.5)
  ----                                 ^^^
    31          >>ELSE
    32              display T-3 (1, 1, 4)
  Error: unsupported literal 0.5
  [1]
  $ CHECK=9 tramabol ko-2.cob
  ko-2.cob:32.30-32.31:
    29          >>ELIF CHECK = 8
    30              display T-3 (1, 2, 0.5)
    31          >>ELSE
    32 >            display T-3 (1, 1, 4)
  ----                                 ^
    33          >>END-IF
    34              stop run.
  Error: Index is out of bounds: got 4, expected in [1..3]
  [1]
