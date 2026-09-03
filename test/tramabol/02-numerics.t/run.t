  $ tramabol integers.cob
  0
  1
  2
  -1
  -2
  2147483647
  -2147483648
  2147483648
  -2147483649
  9223372036854775807
  -9223372036854775808
  9223372036854775808
  -9223372036854775809
  Terminated with status: 0
  $ tramabol integers.cob --show
  { module_memory = <cob_module>; module_unit = <opaque>;
    module_data =
    { map = <opaque>;
      working_storage = { storage_records = []; storage_fields = [] };
      local_storage = { storage_records = []; storage_fields = [] } };
    module_proc =
    [IR_display {
       data_refs =
       [|{ field_ref = (Constant_field 0/int32); field_ref_loc = <opaque> }|];
       advancing = true};
      IR_display {
        data_refs =
        [|{ field_ref = (Constant_field 1/int32); field_ref_loc = <opaque> }|];
        advancing = true};
      IR_display {
        data_refs =
        [|{ field_ref = (Constant_field 2/int32); field_ref_loc = <opaque> }|];
        advancing = true};
      IR_display {
        data_refs =
        [|{ field_ref = (Constant_field -1/int32); field_ref_loc = <opaque> }|];
        advancing = true};
      IR_display {
        data_refs =
        [|{ field_ref = (Constant_field -2/int32); field_ref_loc = <opaque> }|];
        advancing = true};
      IR_display {
        data_refs =
        [|{ field_ref = (Constant_field 2147483647/int32);
            field_ref_loc = <opaque> }
          |];
        advancing = true};
      IR_display {
        data_refs =
        [|{ field_ref = (Constant_field -2147483648/int32);
            field_ref_loc = <opaque> }
          |];
        advancing = true};
      IR_display {
        data_refs =
        [|{ field_ref = (Constant_field 2147483648/int64);
            field_ref_loc = <opaque> }
          |];
        advancing = true};
      IR_display {
        data_refs =
        [|{ field_ref = (Constant_field -2147483649/int64);
            field_ref_loc = <opaque> }
          |];
        advancing = true};
      IR_display {
        data_refs =
        [|{ field_ref = (Constant_field 9223372036854775807/int64);
            field_ref_loc = <opaque> }
          |];
        advancing = true};
      IR_display {
        data_refs =
        [|{ field_ref = (Constant_field -9223372036854775808/int64);
            field_ref_loc = <opaque> }
          |];
        advancing = true};
      IR_display {
        data_refs =
        [|{ field_ref = (Constant_field "9223372036854775808");
            field_ref_loc = <opaque> }
          |];
        advancing = true};
      IR_display {
        data_refs =
        [|{ field_ref = (Constant_field "-922337203685477580");
            field_ref_loc = <opaque> }
          |];
        advancing = true};
      IR_stop {optional_status = None}]
    }
