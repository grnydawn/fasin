# -*- coding: utf-8 -*-
from parsimonious.grammar import Grammar
from parsimonious.nodes import NodeVisitor
import os, sys
from . import utils

sys.setrecursionlimit(500)

f2003_grammar = Grammar(
    r"""
        # From J3/04-007(Fortran 2003)
        ################## Fortran high-level concepts ###################
        program                 = program_unit+
        program_unit            = main_program / external_subprogram / module /
                                  block_data / _CL
        main_program            = program_stmt? specification_part? execution_part?
                                  (_CL* internal_subprogram_part)? _CL* end_program_stmt
        external_subprogram     = function_subprogram / subroutine_subprogram
        function_subprogram     = function_stmt specification_part? execution_part?
                                  (_CL* internal_subprogram_part)? _CL* end_function_stmt
        subroutine_subprogram   = subroutine_stmt specification_part? execution_part?
                                  (_CL* internal_subprogram_part)? _CL* end_subroutine_stmt
        module                  = module_stmt specification_part?
                                  (_CL* module_subprogram_part)? _CL* end_module_stmt
        block_data              = block_data_stmt specification_part?
                                  _CL* end_block_data_stmt
        specification_part      = (use_stmt _CL*)*  (import_stmt _CL*)*
                                  implicit_part* declaration_construct*
        implicit_part           = implicit_stmt / implicit_part_stmt*
        implicit_part_stmt      = implicit_stmt / parameter_stmt /
                                  format_stmt / entry_stmt / _CL
        declaration_construct   = derived_type_def / entry_stmt / enum_def /
                                  format_stmt / interface_block / parameter_stmt /
                                  procedure_declaration_stmt / specification_stmt /
                                  type_declaration_stmt / stmt_function_stmt / _CL
        interface_block         = interface_stmt interface_specification*
                                  _CL* end_interface_stmt
        execution_part          = executable_construct execution_part_construct*
        execution_part_construct= executable_construct / format_stmt / entry_stmt /
                                  data_stmt
        internal_subprogram_part= contains_stmt (_CL* internal_subprogram)+
        internal_subprogram     = function_subprogram / subroutine_subprogram
        module_subprogram_part  = contains_stmt _CL* (internal_subprogram _CL*)+
        module_subprogram       = function_subprogram / subroutine_subprogram
        specification_stmt      = access_stmt / allocatable_stmt / asynchronous_stmt /
                                  bind_stmt / common_stmt / data_stmt /
                                  dimension_stmt / equivalence_stmt /
                                  external_stmt / intent_stmt / intrinsic_stmt /
                                  namelist_stmt / optional_stmt / pointer_stmt /
                                  protected_stmt / save_stmt / target_stmt /
                                  volatile_stmt / value_stmt / _CL
        executable_construct    = action_stmt / associate_construct / case_construct /
                                  do_construct / forall_construct / if_construct /
                                  select_type_construct / where_construct
        action_stmt             = allocate_stmt / assignment_stmt / backspace_stmt /
                                  call_stmt / close_stmt / continue_stmt / cycle_stmt /
                                  deallocate_stmt / endfile_stmt / exit_stmt /
                                  flush_stmt / forall_stmt / goto_stmt / if_stmt /
                                  inquire_stmt / nullify_stmt / open_stmt /
                                  pointer_assignment_stmt / print_stmt / read_stmt /
                                  return_stmt / rewind_stmt / stop_stmt / wait_stmt /
                                  where_stmt / write_stmt / arithmetic_if_stmt /
                                  computed_goto_stmt /_CL

        ################## constructs and definitions ###################
        if_construct            = if_then_stmt _CL* block (_CL* else_if_stmt _CL* block)*
                                  (_CL* else_stmt _CL* block)? _CL* end_if_stmt
        do_construct            = block_do_construct / nonblock_do_construct
        block_do_construct      = do_stmt do_block end_do
        nonblock_do_construct   = action_term_do_construct / outer_shared_do_construct
        action_term_do_construct= label_do_stmt do_body do_term_action_stmt
        outer_shared_do_construct= label_do_stmt do_body shared_term_do_construct
        shared_term_do_construct= outer_shared_do_construct / inner_shared_do_construct
        inner_shared_do_construct= label_do_stmt do_body do_term_shared_construct
        do_term_shared_construct= action_stmt
        where_construct         = where_construct_stmt (_CL* where_body_construct)*
                                  (_CL* masked_elsewhere_stmt (_CL* where_body_construct)*)*
                                  (_CL* elsewhere_stmt (_CL* where_body_construct )*)?
                                  _CL* end_where_stmt
        where_body_construct    = where_assignment_stmt / where_stmt / where_construct
        forall_construct        = forall_construct_stmt (_CL* forall_body_construct)* _CL*
                                  end_forall_stmt
        forall_body_construct   = forall_assignment_stmt / where_stmt / where_construct /
                                  forall_construct / forall_stmt
        select_type_construct   = select_type_stmt (_CL* type_guard_stmt block)* _CL*
                                  end_select_type_stmt
        case_construct          = select_case_stmt (_CL* case_stmt block)* _CL* end_select_stmt
        associate_construct     = associate_stmt _CL* block _CL* end_associate_stmt
        derived_type_def        = derived_type_stmt (_CL* type_param_def_stmt)*
                                  (_CL* private_or_sequence)* component_part?
                                  type_bound_procedure_part? _CL* end_type_stmt
        enum_def                = enum_def_stmt (_CL* enumerator_def_stmt)+ _CL* end_enum_stmt

        ################## statement groups ###################
        do_block                = block
        block                   = execution_part_construct*
        do_body                 = execution_part_construct*
        component_part          = (_CL* component_def_stmt)*
        type_bound_procedure_part = _CL* contains_stmt (_CL* binding_private_stmt)? _CL*
                                  proc_binding_stmt (_CL* proc_binding_stmt)*
        interface_specification = interface_body / procedure_stmt
        interface_body          = (function_stmt specification_part? _CL* end_function_stmt) /
                                  (subroutine_stmt specification_part? _CL* end_subroutine_stmt)
        end_do                  = end_do_stmt / continue_stmt
        private_or_sequence     = private_components_stmt / sequence_stmt

        ################## statements ###################
        program_stmt            = _0 ~"PROGRAM"i _1 program_name _CL
        module_stmt             = _0 ~"MODULE"i _1 module_name _CL
        interface_stmt          = (_0 ~"INTERFACE"i (_1 generic_spec)? _CL) / (_0 ~"ABSTRACT"i _1
                                  ~"INTERFACE"i _CL)
        block_data_stmt         = _0 ~"BLOCK"i _0 ~"DATA"i (_1 block_data_name)? _CL
        use_stmt                = (_0 ~"USE"i ((_0 "," _0 module_nature)? _0 "::")? &(":" / ~"[ \t]") _0 module_name
                                  (_0 "," _0 rename_list )? _CL) /
                                  (_0 ~"USE"i ((_0 "," _0 module_nature)? _0 "::")? &(":" / ~"[ \t]") _0 module_name
                                  _0 "," _0 ~"ONLY"i _0 ":" (_0 only_list)? _CL)
        implicit_stmt           = (_0 ~"IMPLICIT"i _1 implicit_spec_list _CL) /
                                  (_0 ~"IMPLICIT"i _1 ~"NONE"i _CL)
        function_stmt           = _0 (prefix _1)? ~"FUNCTION"i _1 function_name _0
                                  "(" _0 dummy_arg_name_list? _0 ")" (_0 suffix)? _CL
        contains_stmt           = _0 ~"CONTAINS"i _CL
        subroutine_stmt         = _0 (prefix _1)? ~"SUBROUTINE"i _1 subroutine_name (_0 "("
                                  (_0 dummy_arg_list)? _0 ")" (_0 proc_language_binding_spec)?)? _CL
        enum_def_stmt           = _0 ~"ENUM"i _0 "," _0  ~"BIND"i _0 "(" _0 ~"C"i _0 ")" _CL
        enumerator_def_stmt     = _0 ~"ENUMERATOR"i (_0 "::")? &(":" / ~"[ \t]") _0 enumerator_list _CL
        if_then_stmt            = _0 (if_construct_name ":" _0)? ~"IF"i _0 "(" _0
                                  scalar_logical_expr _0 ")" _0 ~"THEN"i _CL
        else_if_stmt            = _0 ~"ELSE"i _0 ~"IF"i _0 "(" _0 scalar_logical_expr _0 ")" _0
                                  ~"THEN"i (_0 if_construct_name)? _CL
        else_stmt               = _0 ~"ELSE"i (_1 if_construct_name)? _CL
        do_term_action_stmt     = action_stmt
        where_construct_stmt    = _0 (where_construct_name _0 ":" _0)? ~"WHERE"i _0 "(" _0
                                  mask_expr _0 ")" _CL
        where_assignment_stmt   = assignment_stmt
        masked_elsewhere_stmt   = _0 ~"ELSEWHERE"i _0 "(" _0 mask_expr _0 ")"
                                  (_0 where_construct_name)? _CL
        elsewhere_stmt          = _0 ~"ELSEWHERE"i (_0 where_construct_name)? _CL
        forall_construct_stmt   = _0 (forall_construct_name _0 ":" _0)? ~"FORALL"i _0
                                  forall_header _CL
        forall_assignment_stmt  = pointer_assignment_stmt / assignment_stmt
        select_type_stmt        = _0 (select_construct_name _0 ":" _0)? ~"SELECT"i _0 ~"TYPE"i _0 "("
                                  (_0 associate_name _0 "=>")? _0 selector ")"
        select_case_stmt        = _0 (case_construct_name _0 ":" _0)? ~"SELECT"i _0 ~"CASE"i _0 "("
                                  _0 case_expr _0 ")"
        case_stmt               = _0 ~"CASE"i _1 case_selector (_0 case_construct_name)?
        associate_stmt          = _0 (associate_construct_name _0 ":" _0)? ~"ASSOCIATE"i _0 "(" _0
                                  association_list _0 ")"
        procedure_declaration_stmt= _0 ~"PROCEDURE"i _0 "(" (_0 proc_interface )? _0 ")" ((_0 "," _0
                                  proc_attr_spec )* _0 "::")? _0 proc_decl_list _CL
        import_stmt             = _0 ~"IMPORT"i ((_0 "::")? &(":" / ~"[ \t]") _0 import_name_list )? _CL
        access_stmt             = _0 access_spec ((_0 "::")? &(":" / ~"[ \t]") _0 access_id_list )? _CL
        allocatable_stmt        = _0 ~"ALLOCATABLE"i (_0 "::" )? &(":" / ~"[ \t]") _0 object_name
                                  (_0 "(" _0 deferred_shape_spec_list _0 ")")? (_0 "," _0 object_name
                                  (_0 "(" _0 deferred_shape_spec_list _0 ")")?)* _CL
        asynchronous_stmt       = _0 ~"ASYNCHRONOUS"i (_0 "::")? &(":" / ~"[ \t]") _0 object_name_list _CL
        bind_stmt               = _0 language_binding_spec (_0 "::")? &(":" / ~"[ \t]") _0 bind_entity_list _CL
        common_stmt             = _0 ~"COMMON"i (_1 "/" (_0 common_block_name )? _0 "/")? _0
                                  common_block_object_list ((_0 "," )? _0 "/" (_0 common_block_name )?
                                  _0 "/" _0 common_block_object_list)* _CL
        data_stmt               = _0 ~"DATA"i _1 data_stmt_set ( (_0 ",")? _0 data_stmt_set )*
        dimension_stmt          = _0 ~"DIMENSION"i (_0 "::")? &(":" / ~"[ \t]") _0 array_name _0 "(" _0
                                  array_spec _0 ")" (_0 "," _0 array_name _0 "(" _0 array_spec _0 ")")* _CL
        equivalence_stmt        = _0 ~"EQUIVALENCE"i _0 equivalence_set_list
        external_stmt           = _0 ~"EXTERNAL"i (_0 "::")? &(":" / ~"[ \t]") _0 external_name_list _CL
        intent_stmt             = _0 ~"INTENT" _0 "(" _0 intent_spec _0 ")" (_0 "::")? &(":" / ~"[ \t]")
                                  _0 dummy_arg_name_list _CL
        intrinsic_stmt          = _0 ~"INTRINSIC" (_0 "::")? &(":" / ~"[ \t]") _0 intrinsic_procedure_name_list _CL
        namelist_stmt           = "$"
        optional_stmt           = "$"
        pointer_stmt            = "$"
        protected_stmt          = "$"
        save_stmt               = _0 ~"SAVE"i ((_0 "::")? &(":" / ~"[ \t]") _0 saved_entity_list)? _CL
        target_stmt             = "$"
        volatile_stmt           = "$"
        value_stmt              = "$"
        do_stmt                 = label_do_stmt / nonlabel_do_stmt
        nonlabel_do_stmt        = _0 (do_construct_name _0 ":")? _0 ~"DO"i _1 loop_control? _CL
        label_do_stmt           = _0 (do_construct_name _0 ":")? _0 ~"DO"i _1 label _1
                                  loop_control? _CL
        allocate_stmt           = "$"
        assignment_stmt         = _0 variable _0 "=" _0 expr _CL
        backspace_stmt          = "$"
        call_stmt               = "$"
        close_stmt              = "$"
        continue_stmt           = _0 ~"CONTINUE"i _CL
        cycle_stmt              = "$"
        deallocate_stmt         = "$"
        exit_stmt               = _0 ~"EXIT" (_0 do_construct_name)? _CL
        flush_stmt              = "$"
        forall_stmt             = "$"
        goto_stmt               = "$"
        if_stmt                 = "$"
        inquire_stmt            = "$"
        nullify_stmt            = "$"
        open_stmt               = "$"
        pointer_assignment_stmt = (_0 proc_pointer_object _0 "=>" _0 proc_target _CL) /
                                  (_0 data_pointer_object (_0 "(" _0 bounds_spec_list _0 ")")? _0 "=>"
                                  _0 data_target _CL) / (_0 data_pointer_object _0 "(" _0
                                  bounds_remapping_list _0 ")" _0 "=>" _0 data_target _CL)
        print_stmt              = _0 ~"PRINT"i _1 format (_0 "," _0 output_item_list)? _CL
        read_stmt               = "$"
        return_stmt             = _0 ~"RETURN" (_0 scalar_int_expr)? _CL
        rewind_stmt             = "$"
        endfile_stmt            = "$"
        stop_stmt               = _0 ~"STOP"i (_0 stop_code)? _CL
        wait_stmt               = "$"
        where_stmt              = _0 ~"WHERE"i _0 "(" _0 mask_expr _0 ")" _0 where_assignment_stmt _CL
        write_stmt              = _0 ~"WRITE"i _0 "(" _0 io_control_spec_list _0 ")" (_0 output_item_list)?
        arithmetic_if_stmt      = "$"
        computed_goto_stmt      = "$"
        stmt_function_stmt      = "$"
        type_declaration_stmt   = _0 declaration_type_spec ((_0 "," _0 attr_spec )* _0 "::" )?
                                  &(":" / ~"[ \t]") _0 entity_decl_list _CL
        parameter_stmt          = "$"
        format_stmt             = _0 ~"FORMAT"i _0 format_specification _CL
        entry_stmt              = "$"
        procedure_stmt          = _0 (~"MODULE"i _1)? ~"PROCEDURE"i _1 procedure_name_list _CL
        private_components_stmt = _0 ~"PRIVATE"i _CL
        sequence_stmt           = _0 ~"SEQUENCE"i _CL
        component_def_stmt      = data_component_def_stmt / proc_component_def_stmt
        data_component_def_stmt = _0 declaration_type_spec ((_0 "," component_attr_spec_list)?
                                  _0 "::")? &(":" / ~"[ \t]") _0 component_decl_list _CL
        proc_component_def_stmt = _0 ~"PROCEDURE"i _0 "(" (_0 proc_interface)? _0 ")" _0 "," _0
                                  proc_component_attr_spec_list _0 "::" _0 proc_decl_list _CL
        derived_type_stmt       = _0 ~"TYPE"i ((_0 "," _0 type_attr_spec_list)? _0 "::")? &(":" / ~"[ \t]") _0
                                  type_name (_0 "(" _0 type_param_name_list _0 ")")? _CL
        type_param_def_stmt     = _0 ~"INTEGER"i (_0 kind_selector)? _0 "," _0 type_param_attr_spec
                                  _0 "::" _0 type_param_decl_list _CL
        binding_private_stmt    = _0 ~"PRIVATE"i _CL
        proc_binding_stmt       = (_0 specific_binding _CL) / (_0 generic_binding _CL) /
                                  (_0 final_binding _CL)
        type_guard_stmt         = (_0 ~"TYPE"i _1 ~"IS"i _0 "(" _0 type_spec _0 ")" (_0 select_construct_name)? _CL)
                                  / (_0 ~"CLASS"i _1 ~"IS"i _0 "(" _0 type_spec _0 ")"
                                  (_0 select_construct_name)? _CL) / (_0 ~"CLASS"i _1 ~"DEFAULT"i
                                  (_0 select_construct_name )? _CL)

        ################## end statements ###################
        end_program_stmt        = _0 ~"END"i (_0 ~"PROGRAM"i (_1 program_name)?)? _CL
        end_module_stmt         = _0 ~"END"i (_0 ~"MODULE"i (_1 module_name)?)? _CL
        end_block_data_stmt     = _0 ~"END"i (_0 ~"BLOCK"i _0 ~"DATA"i (_1 block_data_name)?)? _CL
        end_function_stmt       = _0 ~"END"i (_0 ~"FUNCTION"i (_1 function_name)?)? _CL
        end_subroutine_stmt     = _0 ~"END"i (_0 ~"SUBROUTINE"i (_1 subroutine_name)?)? _CL
        end_interface_stmt      = _0 ~"END"i _0 ~"INTERFACE"i (_1 generic_spec)? _CL
        end_do_stmt             = _0 ~"END"i _0 ~"DO"i (_1 do_construct_name)? _CL
        end_if_stmt             = _0 ~"END"i _0 ~"IF"i (_1 if_construct_name)? _CL
        end_type_stmt           = _0 ~"END"i _0 ~"TYPE"i (_1 type_name)? _CL
        end_where_stmt          = _0 ~"END"i _0 ~"WHERE"i (_1 where_construct_name)? _CL
        end_forall_stmt         = _0 ~"END"i _0 ~"FORALL"i (_1 forall_construct_name)? _CL
        end_select_stmt         = _0 ~"END"i _0 ~"SELECT"i (_1 select_construct_name)? _CL
        end_associate_stmt      = _0 ~"END"i _0 ~"ASSOCIATE"i (_1 associate_construct_name)? _CL
        end_enum_stmt           = _0 ~"END"i _0 ~"ENUM"i _CL
        end_select_type_stmt    = end_select_stmt

        ################## expressions and operands ###################
        case_expr               = scalar_int_expr / scalar_char_expr / scalar_logical_expr
        scalar_logical_initialization_expr = logical_initialization_expr
        scalar_char_initialization_expr = char_initialization_expr
        logical_initialization_expr= logical_expr
        char_initialization_expr= char_expr
        scalar_char_expr        = char_expr
        upper_bound_expr        = scalar_int_expr
        lower_bound_expr        = scalar_int_expr
        scalar_int_expr         = int_expr
        scalar_logical_expr     = logical_expr
        scalar_mask_expr        = mask_expr
        mask_expr               = logical_expr
        specification_expr      = scalar_int_expr
        scalar_default_char_expr= default_char_expr
        default_char_expr       = expr
        char_expr               = expr
        logical_expr            = expr
        int_expr                = expr
        initialization_expr     = expr
        scalar_int_initialization_expr = expr
        expr                    = (_expr _0 defined_binary_op _0 level_5_expr) / _expr / level_5_expr
        _expr                   = level_5_expr _0 defined_binary_op _0 level_5_expr
        level_5_expr            = (_level_5_expr _0 equiv_op _0 equiv_operand) / _level_5_expr / equiv_operand
        _level_5_expr           = equiv_operand _0 equiv_op _0 equiv_operand
        equiv_operand           = (_equiv_operand _0 or_op _0 or_operand) / _equiv_operand / or_operand
        _equiv_operand          = or_operand _0 or_op _0 or_operand
        or_operand              = (_or_operand _0 and_op _0 and_operand) / _or_operand / and_operand
        _or_operand             = and_operand _0 and_op _0 and_operand
        and_operand             = (not_op _0)? level_4_expr
        level_4_expr            = (level_3_expr _0 rel_op _0)? level_3_expr
        level_3_expr            = (_level_3_expr _0 concat_op _0 level_2_expr) / _level_3_expr / level_2_expr
        _level_3_expr           = level_2_expr _0 concat_op _0 level_2_expr
        level_2_expr            = (_level_2_expr _0 add_op _0 add_operand) / (add_op _level_2_expr) / _level_2_expr /  add_operand
        _level_2_expr           = (add_operand _0 add_op _0 add_operand) / (add_op _0 add_operand)
        add_operand             = (_add_operand  _0 mult_op _0 mult_operand) / _add_operand / mult_operand
        _add_operand            = mult_operand _0 mult_op _0 mult_operand
        mult_operand            = (level_1_expr _0 power_op _0 _mult_operand) / _mult_operand / level_1_expr
        _mult_operand           = level_1_expr _0 power_op _0 level_1_expr
        level_1_expr            = (defined_unary_op _0)? primary
        primary                 = array_constructor / structure_constructor / function_reference /
                                  type_param_inquiry / ("(" _0 expr _0 ")") / designator / constant /
                                  type_param_name


        ################## specs ###################
        proc_attr_spec          = ~"SAVE"i / ~"POINTER"i / (~"INTENT"i _0 "(" _0 intent_spec _0 ")") /
                                  access_spec / proc_language_binding_spec
        bounds_spec             = lower_bound_expr _0 ":"
        forall_triplet_spec     = index_name _0 "=" _0 subscript _0 ":" _0 subscript (_0 ":" _0 stride)?
        prefix_spec             = ~"RECURSIVE"i / ~"PURE"i / ~"ELEMENTAL"i / declaration_type_spec
        proc_language_binding_spec = language_binding_spec
        type_attr_spec          = access_spec / (~"EXTENDS"i _0 "(" _0 parent_type_name _0 ")") /
                                  ~"ABSTRACT"i / (~"BIND"i _0 "(" _0 "C" _0 ")")
        type_param_attr_spec    = ~"KIND"i / ~"LEN"i
        type_param_decl         = type_param_name (_0 "=" _0 scalar_int_initialization_expr )?
        component_attr_spec     = ~"POINTER"i / (~"DIMENSION"i _0 "(" _0 component_array_spec _0 ")") /
                                  ~"ALLOCATABLE"i / access_spec
        component_array_spec    = explicit_shape_spec_list / deferred_shape_spec_list
        proc_component_attr_spec= ~"POINTER"i / (~"PASS"i ( _0 "(" _0 arg_name _0 ")")?) /
                                  ~"NOPASS"i / access_spec
        generic_spec            = (~"ASSIGNMENT"i _0 "(" _0 "=" _0 ")") /
                                  (~"OPERATOR"i _0 "(" _0 defined_operator _0 ")") /
                                  dtio_generic_spec / generic_name
        dtio_generic_spec       = (~"READ"i _0 "(" _0 ~"FORMATTED"i _0 ")") /
                                  (~"READ"i _0 "(" _0 ~"UNFORMATTED"i _0 ")") /
                                  (~"WRITE"i _0 "(" _0 ~"FORMATTED"i _0 ")") /
                                  (~"WRITE"i _0 "(" _0 ~"UNFORMATTED"i _0 ")")
        io_control_spec         = ((~"UNIT"i _0 "=" _0)? io_unit) / ((~"FMT"i _0 "=" _0)? format) /
                                  ((~"NML"i _0 "=" _0)? namelist_group_name) /
                                  (~"ADVANCE"i _0 "=" _0 scalar_default_char_expr) /
                                  (~"ASYNCHRONOUS"i _0 "=" _0 scalar_char_initialization_expr) /
                                  (~"BLANK"i _0 "=" _0 scalar_default_char_expr) /
                                  (~"DECIMAL"i _0 "=" _0 scalar_default_char_expr) /
                                  (~"DELIM"i _0 "=" _0 scalar_default_char_expr) /
                                  (~"END"i _0 "=" _0 label) / (~"EOR"i _0 "=" _0 label) /
                                  (~"ERR"i _0 "=" _0 label) /  (~"ID"i _0 "=" _0 scalar_int_variable) /
                                  (~"IOMSG"i _0 "=" _0 iomsg_variable) /
                                  (~"IOSTAT"i _0 "=" _0 scalar_int_variable) /
                                  (~"PAD"i _0 "=" _0 scalar_default_char_expr) /
                                  (~"POS"i _0 "=" _0 scalar_int_expr) /
                                  (~"REC"i _0 "=" _0 scalar_int_expr) /
                                  (~"ROUND"i _0 "=" _0 scalar_default_char_expr) /
                                  (~"SIGN"i _0 "=" _0 scalar_default_char_expr) /
                                  (~"SIZE"i _0 "=" _0 scalar_int_variable)
        array_spec              = explicit_shape_spec_list / assumed_shape_spec_list /
                                  deferred_shape_spec_list / assumed_size_spec
        attr_spec               = access_spec / ~"ALLOCATABLE"i / ~"ASYNCHRONOUS"i /
                                  (~"DIMENSION"i _0 "(" _0 array_spec _0 ")") / ~"EXTERNAL"i /
                                  (~"INTENT"i _0 "(" _0 intent_spec _0 ")") / ~"INTRINSIC"i /
                                  language_binding_spec / ~"OPTIONAL"i / ~"PARAMETER"i /
                                  ~"POINTER"i / ~"PROTECTED"i / ~"SAVE"i / ~"TARGET"i /
                                  ~"VALUE"i / ~"VOLATILE"i
        access_spec             = ~"PUBLIC"i / ~"PRIVATE"i
        explicit_shape_spec     = (lower_bound _0 ":" _0)? upper_bound
        ac_spec                 = (type_spec _0 "::") / ((type_spec _0 "::")? &(":" / ~"[ \t]") ac_value_list)
        type_spec               = intrinsic_type_spec / derived_type_spec
        derived_type_spec       = type_name (_0 "(" _0 type_param_spec_list _0 ")")?
        type_param_spec         = (keyword _0 "=" _0)? type_param_value
        assumed_shape_spec      = (lower_bound _0)? ":"
        deferred_shape_spec     = ":"
        assumed_size_spec       = (explicit_shape_spec_list _0 ",")? (_0 lower_bound _0 ":" _0)? "*"
        intent_spec             = ~"IN"i / ~"OUT"i / ~"INOUT"i
        language_binding_spec   = ~"BIND"i _0 "(" _0 "C" (_0 "," _0 ~"NAME"i _0 "=" _0
                                  scalar_char_initialization_expr )? _0 ")"
        component_spec          = (keyword _0 "=" _0)? component_data_source
        actual_arg_spec         = (keyword _0 "=" _0)? actual_arg
        alt_return_spec         = "*" _0 label
        declaration_type_spec   = (~"TYPE"i _0 "(" _0 derived_type_spec _0 ")") /
                                  (~"CLASS"i _0 "(" _0 derived_type_spec _0 ")") /
                                  (~"CLASS"i _0 "(" _0 "*" _0 ")") / intrinsic_type_spec
        intrinsic_type_spec     = (~"INTEGER"i (_0 kind_selector)?) / (~"REAL"i (_0 kind_selector)?) /
                                  (~"DOUBLE"i _0 ~"PRECISION"i) / (~"COMPLEX" (_0 kind_selector)?) /
                                  (~"CHARACTER"i (_0 char_selector)?) / (~"LOGICAL"i (_0 kind_selector)?)
        implicit_spec           = declaration_type_spec _0 "(" _0 letter_spec_list _0 ")"

        ##########
#        data_edit_desc          = (~"I"i _0 w (_0 "." _0 m)?) / (~"B"i _0 w (_0 "." _0 m)?) /
#                                  (~"O"i _0 w (_0 "." _0 m)?) / (~"Z"i _0 w (_0 "." _0 m)?) /
#                                  (~"F"i _0 w _0 "." _0 d) / (~"E"i _0 w _0 "." _0 d (_0 ~"E"i _0 e)?) /
#                                  (~"EN"i _0 w _0 "." _0 d (_0 "E" _0 e)?) / (~"ES"i _0 w _0 "." _0 d (_0 ~"E"i _0 e)?) /
#                                  (~"G"i _0 w _0 "." _0 d (_0 "E" _0 e)?) / (~"L"i _0 w) / (~"A"i (_0 w)?) /
#                                  (~"D"i _0 w _0 "." _0 d) / (~"DT"i (_0 char_literal_constant)? (_0 "(" _0 v_list _0 ")")?)
#        r                       = int_literal_constant
#        w                       = int_literal_constant
#        m                       = int_literal_constant
#        d                       = int_literal_constant
#        e                       = int_literal_constant
#        v                       = signed_int_literal_constant
#        control_edit_desc       = position_edit_desc / ((r _0)? "/") / ":" / sign_edit_desc / (k _0 ~"P"i) / blank_interp_edit_desc / round_edit_desc / decimal_edit_desc
#        position_edit_desc      = (~"T"i _0 n) / (~"TL"i _0 n) / (~"TR"i _0 n) / (n _0 ~"X")
#        n                       = int_literal_constant
#        sign_edit_desc          = ~"SS"i / ~"SP"i / ~"S"i
#        k                       = signed_int_literal_constant
#        blank_interp_edit_desc  = ~"BN"i / ~"BZ"i
#        round_edit_desc         = ~"RU"i / ~"RD"i / ~"RZ"i / ~"RN"i / ~"RC"i / ~"RP"i
#        decimal_edit_desc       = ~"DC"i / ~"DP"i
#        char_string_edit_desc   = char_literal_constant

        ################## sub-expressions ###################
        # TODO: refine format specification
        format_specification    = "(" (_0 format_item_list)? _0 ")"
        format_item             =  _combined_format_item / char_string_edit_desc / control_edit_desc /
                                  ((!~"P"i !~"X"i !"/" r)? "(" _0 format_item_list _0 ")") /
                                  ((!~"P"i !~"X"i !"/" r)? data_edit_desc)

        _combined_format_item   =  _p_combined / _before_after_slash /_colon
        _p_combined             = r? ~"P"i ((~"F"i w "." d) / (~"E"i w "." d (~"E"i e)?) /
                                  (~"EN"i w "." d (~"E"i e)?) / (~"ES"i w "." d (~"E"i e)?) /
                                  (~"G"i w "." d (~"E"i e)?) / (~"D"i w "." d))
        _colon                  = ((char_string_edit_desc / control_edit_desc /
                                  ((!~"P"i !~"X"i !"/" r)? "(" _0 format_item_list _0 ")") /
                                  ((!~"P"i !~"X"i !"/" r)? data_edit_desc))+ ":") /
                                  (":" (char_string_edit_desc / control_edit_desc /
                                  ((!~"P"i !~"X"i !"/" r)? "(" _0 format_item_list _0 ")") /
                                  ((!~"P"i !~"X"i !"/" r)? data_edit_desc))+)
        _before_after_slash     = ((char_string_edit_desc / (~"T"i n) / (~"TL"i n) / (~"TR"i n) / (n ~"X"i) /
                                  ( "(" _0 format_item_list _0 ")") /
                                  ( data_edit_desc))+ "/") /
                                  ("/" (char_string_edit_desc / control_edit_desc /
                                  ((!~"P"i !~"X"i !"/" r)? "(" _0 format_item_list _0 ")") /
                                  ((!~"P"i !~"X"i !"/" r)? data_edit_desc))+)

        data_edit_desc          = (~"I"i w ("." m)?) / (~"B"i w ("." m)?) /
                                  (~"O"i w ("." m)?) / (~"Z"i w ("." m)?) /
                                  (~"F"i w "." d) / (~"E"i w "." d (~"E"i e)?) /
                                  (~"EN"i w "." d (~"E"i e)?) / (~"ES"i w "." d (~"E"i e)?) /
                                  (~"G"i w "." d (~"E"i e)?) / (~"L"i w) / (~"A"i w?) /
                                  (~"D"i w "." d) / (~"DT"i (char_literal_constant)? ("(" _0 v_list _0 ")")?)
        r                       = int_literal_constant
        w                       = int_literal_constant
        m                       = int_literal_constant
        d                       = int_literal_constant
        e                       = int_literal_constant
        n                       = int_literal_constant
        v                       = signed_int_literal_constant
        k                       = signed_int_literal_constant
        control_edit_desc       = position_edit_desc / ((!~"P"i !~"X"i !"(" r)? "/") / ":" / sign_edit_desc /
                                  (k ~"P"i) / blank_interp_edit_desc / round_edit_desc / decimal_edit_desc
        position_edit_desc      = (~"T"i n) / (~"TL"i n) / (~"TR"i n) / (n ~"X"i)
        sign_edit_desc          = ~"SS"i / ~"SP"i / ~"S"i
        blank_interp_edit_desc  = ~"BN"i / ~"BZ"i
        round_edit_desc         = ~"RU"i / ~"RD"i / ~"RZ"i / ~"RN"i / ~"RC"i / ~"RP"i
        decimal_edit_desc       = ~"DC"i / ~"DP"i
        char_string_edit_desc   = char_literal_constant
        selector                = expr / variable
        bind_entity             = ("/" _0 common_block_name _0 "/") / entity_name
        access_id               = generic_spec / use_name
        data_stmt_set           = data_stmt_object_list _0 "/" _0 data_stmt_value_list _0 "/"
        data_stmt_object        = data_implied_do / variable
        data_implied_do         = "(" _0 data_i_do_object_list _0 "," _0 data_i_do_variable _0
                                  "=" _0 scalar_int_expr _0 "," _0 scalar_int_expr (_0 "," _0
                                  scalar_int_expr)? _0 ")"
        data_i_do_object        = array_element / scalar_structure_component / data_implied_do
        data_stmt_value         = (data_stmt_repeat _0 "*"_0)? data_stmt_constant
        data_stmt_repeat        = scalar_int_constant_subobject / scalar_int_constant
        scalar_int_constant_subobject = int_constant_subobject
        scalar_constant_subobject = constant_subobject
        int_constant_subobject  = constant_subobject
        constant_subobject      = designator
        data_stmt_constant      = structure_constructor / null_init/ signed_real_literal_constant /
                                  signed_int_literal_constant / scalar_constant_subobject / scalar_constant
        common_block_object     = (variable_name (_0 "(" _0 explicit_shape_spec_list _0 ")")?) /
                                  proc_pointer_name
        data_pointer_object     = (variable _0 "%" _0 data_pointer_component_name) / variable_name
        proc_pointer_object     = proc_component_ref / proc_pointer_name
        bounds_remapping        = lower_bound_expr _0 ":" _0 upper_bound_expr
        forall_header           = "(" _0 forall_triplet_spec_list (_0 "," _0 scalar_mask_expr)? _0 ")"
        association             = associate_name _0 "=>" _0 selector
        case_selector           = ~"DEFAULT"i / ("(" _0 case_value_range_list _0 ")")
        case_value_range        = case_value / (case_value _0 ":") / (":" _0 case_value) /
                                  (case_value _0 ":" _0 case_value)
        case_value              = scalar_int_initialization_expr / scalar_char_initialization_expr /
                                  scalar_logical_initialization_expr
        designator              = array_section / array_element / structure_component /
                                  substring / object_name
        module_nature           = ~"NON_INTRINSIC"i / ~"INTRINSIC"i
        rename                  = (local_name _0 "=>" _0 use_name) / (~"OPERATOR"i _0 "(" _0
                                  local_defined_operator _0 ")" _0 "=>" ~"OPERATOR"i _0 "(" _0
                                  use_defined_operator _0 ")")
        only                    = generic_spec / rename / only_use_name
        prefix                  = prefix_spec+
        suffix                  = (proc_language_binding_spec (_1 ~"RESULT"i _0 "(" _0
                                  result_name _0 ")")?) / (~"RESULT"i _0 "(" _0
                                  result_name _0 ")" (_0 proc_language_binding_spec)?)
        dummy_arg               = "*" / dummy_arg_name
        component_decl          = component_name (_0 "(" _0 component_array_spec _0 ")")?
                                  (_0 "*" _0 char_length)? (_0 component_initialization)?
        component_initialization= initialization
        proc_interface          = (interface_name !(_0 "(")) / declaration_type_spec
        proc_decl               = procedure_entity_name (_0 "=>" _0 null_init)?
        specific_binding        = ~"PROCEDURE"i (_0 "(" _0 interface_name _0 ")")? ((_0 "," _0
                                  binding_attr_list)? _0 "::" _0)? &(":" / ~"[ \t]" / ")") binding_name (_0 "=>" _0 procedure_name)?
        binding_attr            = (~"PASS"i (_0 "(" _0 arg_name _0 ")")?) / ~"NOPASS"i / ~"NON_OVERRIDABLE"i /
                                  ~"DEFERRED"i / access_spec
        generic_binding         = ~"GENERIC"i (_0 "," _0 access_spec _0)? "::" _0 generic_spec _0 "=>" _0
                                  binding_name_list
        final_binding           = ~"FINAL"i (_0 "::")? &(":" / ~"[ \t]") _0 final_subroutine_name_list
        enumerator              = named_constant (_0 "=" _0 scalar_int_initialization_expr)?
        saved_entity            = ("/" _0 common_block_name _0 "/" ) / proc_pointer_name /
                                  object_name
        array_constructor       = ("(/" _0 ac_spec _0 "/)") / ("[" _0 ac_spec _0 "]")
        structure_constructor   = derived_type_spec _0 "(" (_0 component_spec_list)? _0 ")"
        loop_control            = (("," _0)? do_variable _0 "=" _0 scalar_int_expr _0
                                  "," _0 scalar_int_expr (_0 "," _0 scalar_int_expr )?) /
                                  (("," _0)? ~"WHILE"i _0 "(" _0 scalar_logical_expr _0 ")")
        io_unit                 = "*" / file_unit_number / internal_file_variable
        file_unit_number        = scalar_int_expr
        format                  = "*" / default_char_expr / label
        stop_code               = scalar_char_constant / ~"[0-9]{{1,5}}"
        output_item             = io_implied_do / expr
        input_item              = io_implied_do / variable
        io_implied_do           = "(" _0 io_implied_do_object_list _0 "," _0 io_implied_do_control _0 ")"
        io_implied_do_object    = input_item / output_item
        io_implied_do_control   = do_variable _0 "=" _0 scalar_int_expr _0 "," _0 scalar_int_expr
                                  (_0 "," _0 scalar_int_expr )?
        entity_decl             = (object_name (_0 "(" _0 array_spec _0 ")")? (_0 "*" _0
                                  char_length)? (_0 initialization)?) / (function_name (_0 "*"
                                  _0 char_length)?)
        lower_bound             = specification_expr
        upper_bound             = specification_expr
        initialization          = ("=" _0 initialization_expr) / ("=>" _0 null_init)
        null_init               = function_reference
        kind_param              = digit_string / scalar_int_constant_name
        significand             = (digit_string "." digit_string?) / ("." digit_string)
        exponent                = signed_digit_string
        signed_digit_string     = (sign _0)? digit_string
        real_part               = signed_int_literal_constant / signed_real_literal_constant /
                                  named_constant
        imag_part               = signed_int_literal_constant / signed_real_literal_constant /
                                  named_constant
        array_element           = data_ref
        data_ref                = part_ref (_0 "%" _0 part_ref)*
        part_ref                = part_name (_0 "(" _0 section_subscript_list _0 ")")?
        section_subscript       = subscript_triplet/ vector_subscript / subscript
        subscript               = scalar_int_expr
        subscript_triplet       = (subscript _0)? ":" (_0 subscript)? (_0 ":" _0 stride)?
        stride                  = scalar_int_expr
        vector_subscript        = int_expr
        array_section           = data_ref (_0 "(" _0 substring_range _0 ")")?
        substring_range         = (scalar_int_expr _0)? ":" (_0 scalar_int_expr)?
        structure_component     = data_ref
        substring               = parent_string _0 "(" _0 substring_range _0 ")"
        parent_string           = array_element / scalar_structure_component / scalar_constant /
                                  scalar_variable_name
        scalar_structure_component = structure_component
        type_param_value        = "*" / ":" / scalar_int_expr
        ac_value                = ac_implied_do / expr
        ac_implied_do           = "(" _0 ac_value_list _0 "," _0 ac_implied_do_control _0 ")"
        ac_implied_do_control   = ac_do_variable _0 "=" _0 scalar_int_expr _0 "," _0 scalar_int_expr
                                  (_0 "," _0 scalar_int_expr)?
        component_data_source   = data_target / proc_target / expr
        data_target             = variable / expr
        proc_target             = proc_component_ref / procedure_name / expr
        proc_component_ref      = variable _0 "%" _0 procedure_component_name
        function_reference      = procedure_designator _0 "(" (_0 actual_arg_spec_list)? _0 ")"
        procedure_designator    = proc_component_ref / (_0 data_ref _0 "%" _0 binding_name) /
                                  procedure_name
        actual_arg              = proc_component_ref / alt_return_spec / variable /
                                  procedure_name / expr
        type_param_inquiry      = designator _0 "%" _0 type_param_name
        char_length             = ("(" _0 type_param_value _0 ")") / scalar_int_literal_constant
        kind_selector           = "(" (_0 ~"KIND"i _0 "=")? _0 scalar_int_initialization_expr _0 ")"
        char_selector           = length_selector / ("(" _0 ~"LEN"i _0 "=" _0 type_param_value _0
                                  "," _0 ~"KIND"i _0 "=" _0 scalar_int_initialization_expr _0 ")") /
                                  ("(" _0 type_param_value _0 "," (_0 ~"KIND"i _0 "=")? _0
                                  scalar_int_initialization_expr _0 ")") / ("(" _0 ~"KIND"i _0 "="
                                  _0 scalar_int_initialization_expr (_0 "," _0 ~"LEN"i _0 "=" _0
                                  type_param_value)? _0 ")")
        length_selector         = ("(" (_0 ~"LEN"i "=")? _0 type_param_value _0 ")") /
                                  ("*" _0 char_length (_0 ",")?)

        equivalence_set         = "(" _0 equivalence_object _0 "," _0 equivalence_object_list _0 ")"
        equivalence_object      = substring / array_element / variable_name

        ################## operators ###################
        defined_operator        = defined_unary_op / defined_binary_op / extended_intrinsic_op
        local_defined_operator  = defined_unary_op / defined_binary_op
        use_defined_operator    = defined_unary_op / defined_binary_op
        extended_intrinsic_op   = intrinsic_operator
        defined_unary_op        = defined_unary_binary_op
        defined_binary_op       = defined_unary_binary_op
        defined_unary_binary_op = !equiv_op !or_op !and_op !not_op !concat_op !add_op !mult_op
                                  !rel_op !~"\.TRUE\."i !~"\.FALSE\."i "." letter+ "."
        intrinsic_operator      = power_op / mult_op / add_op / concat_op / rel_op / not_op /
                                  and_op / or_op / equiv_op
        power_op                = "**"
        mult_op                 = "*" / "/"
        add_op                  = "+" / "-"
        concat_op               = "//"
        rel_op                  = ~"\.EQ\."i / ~"\.NE\."i / ~"\.LT\."i / ~"\.LE\."i / ~"\.GT\."i /
                                  ~"\.GE\."i / "==" / "/=" / "<=" / ">=" / "<" / ">"
        not_op                  = ~"\.NOT\."i
        and_op                  = ~"\.AND\."i
        or_op                   = ~"\.OR\."i
        equiv_op                = ~"\.NEQV\."i / ~"\.EQV\."i

        ################## constants ###################
        scalar_int_constant     = int_constant
        int_constant            = constant
        scalar_constant         = constant
        constant                = literal_constant / named_constant
        literal_constant        = complex_literal_constant / boz_literal_constant /
                                  logical_literal_constant / char_literal_constant /
                                  real_literal_constant / int_literal_constant
        signed_int_literal_constant = (sign _0)? int_literal_constant
        signed_real_literal_constant = (sign _0)? real_literal_constant
        scalar_int_literal_constant = int_literal_constant
        int_literal_constant    = digit_string ("_" kind_param)?
        real_literal_constant   = (significand (exponent_letter exponent)? ("_" kind_param)?) /
                                  (digit_string exponent_letter exponent ("_" kind_param)?)
        complex_literal_constant= "(" _0 real_part _0 "," _0 imag_part _0 ")"
        logical_literal_constant= (~"\.TRUE\."i ("_" kind_param)?) / (~"\.FALSE\."i ("_" kind_param)?)
        char_literal_constant   = (kind_param "_")? rep_char
        boz_literal_constant    = binary_constant / octal_constant / hex_constant
        binary_constant         = (~"B"i "'" ~"[0-1]+" "'") /  (~"B"i ~"[0-1]+" "\"")
        octal_constant          = (~"O"i "'" ~"[0-7]+" "'") /  (~"O"i "\"" ~"[0-7]+" "\"")
        hex_constant            = (~"Z"i "'" ~"[A-F0-9]+"i "'") /  (~"Z"i "\"" ~"[A-F0-9]+"i "\"")
        scalar_char_constant    = rep_char


        ################## lists ###################
        intrinsic_procedure_name_list= intrinsic_procedure_name (_0 "," _0
                                  intrinsic_procedure_name)*
        proc_component_attr_spec_list= proc_component_attr_spec
                                  (_0 "," _0 proc_component_attr_spec)*
        final_subroutine_name_list = final_subroutine_name
                                  (_0 "," _0 final_subroutine_name)*
        io_implied_do_object_list= io_implied_do_object (_0 "," _0 io_implied_do_object)*
        common_block_object_list= common_block_object (_0 "," _0 common_block_object)*
        forall_triplet_spec_list= forall_triplet_spec (_0 "," _0 forall_triplet_spec)*
        component_attr_spec_list= component_attr_spec (_0 "," _0 component_attr_spec)*
        deferred_shape_spec_list= deferred_shape_spec (_0 "," _0 deferred_shape_spec)*
        explicit_shape_spec_list= explicit_shape_spec (_0 "," _0 explicit_shape_spec)*
        assumed_shape_spec_list = assumed_shape_spec (_0 "," _0 assumed_shape_spec)*
        equivalence_object_list = equivalence_object (_0 "," _0 equivalence_object)*
        section_subscript_list  = section_subscript (_0 "," _0 section_subscript)*
        data_stmt_object_list   = data_stmt_object (_0 "," _0 data_stmt_object)*
        bounds_remapping_list   = bounds_remapping (_0 "," _0 bounds_remapping)*
        data_i_do_object_list   = data_i_do_object (_0 "," _0 data_i_do_object)*
        case_value_range_list   = case_value_range (_0 "," _0 case_value_range)*
        data_stmt_value_list    = data_stmt_value (_0 "," _0 data_stmt_value)*
        type_param_spec_list    = type_param_spec (_0 "," _0 type_param_spec)*
        actual_arg_spec_list    = actual_arg_spec (_0 "," _0 actual_arg_spec)*
        type_param_name_list    = type_param_name (_0 "," _0 type_param_name)*
        equivalence_set_list    = equivalence_set (_0 "," _0 equivalence_set)*
        type_param_decl_list    = type_param_decl (_0 "," _0 type_param_decl)*
        io_control_spec_list    = io_control_spec (_0 "," _0 io_control_spec)*
        type_attr_spec_list     = type_attr_spec (_0 "," _0 type_attr_spec)*
        component_decl_list     = component_decl (_0 "," _0 component_decl)*
        procedure_name_list     = procedure_name (_0 "," _0 procedure_name)*
        component_spec_list     = component_spec (_0 "," _0 component_spec)*
        dummy_arg_name_list     = dummy_arg_name (_0 "," _0 dummy_arg_name)*
        external_name_list      = external_name (_0 "," _0 external_name)*
        implicit_spec_list      = implicit_spec (_0 "," _0 implicit_spec)*
        binding_attr_list       = binding_attr (_0 "," _0 binding_attr)*
        binding_name_list       = binding_name (_0 "," _0 binding_name)*
        saved_entity_list       = saved_entity (_0 "," _0 saved_entity)*
        output_item_list        = output_item (_0 "," _0 output_item)*
        entity_decl_list        = entity_decl (_0 "," _0 entity_decl)*
        import_name_list        = import_name (_0 "," _0 import_name)*
        letter_spec_list        = letter_spec (_0 "," _0 letter_spec)*
        bind_entity_list        = bind_entity (_0 "," _0 bind_entity)*
        bounds_spec_list        = bounds_spec (_0 "," _0 bounds_spec)*
        association_list        = association (_0 "," _0 association)*
        object_name_list        = object_name (_0 "," _0 object_name)*
        format_item_list        = format_item (_0 "," _0 format_item)*
        enumerator_list         = enumerator (_0 "," _0 enumerator)*
        proc_decl_list          = proc_decl (_0 "," _0 proc_decl)*
        dummy_arg_list          = dummy_arg (_0 "," _0 dummy_arg)*
        access_id_list          = access_id (_0 "," _0 access_id)*
        ac_value_list           = ac_value (_0 "," _0 ac_value)*
        rename_list             = rename (_0 "," _0 rename)*
        only_list               = only (_0 "," _0 only)*
        v_list                  = v (_0 "," _0 v)*

        ################## variables ###################
        iomsg_variable          = scalar_default_char_variable
        data_i_do_variable      = scalar_int_variable
        do_variable             = scalar_int_variable
        ac_do_variable          = scalar_int_variable
        scalar_int_variable     = int_variable
        scalar_default_char_variable = default_char_variable
        internal_file_variable  = char_variable
        default_char_variable   = variable
        char_variable           = variable
        int_variable            = variable
        variable                = designator

        ################## names ###################
        scalar_variable_name    = variable_name
        data_pointer_component_name = name
        program_name            = name
        module_name             = name
        result_name             = name
        dummy_arg_name          = name
        subroutine_name         = name
        do_construct_name       = name
        function_name           = name
        named_constant          = name
        scalar_int_constant_name= name
        object_name             = name
        part_name               = name
        variable_name           = name
        type_name               = name
        keyword                 = name
        procedure_component_name= name
        procedure_name          = name
        binding_name            = name
        type_param_name         = name
        common_block_name       = name
        proc_pointer_name       = name
        generic_name            = name
        local_name              = name
        use_name                = name
        import_name             = name
        only_use_name           = name
        parent_type_name        = name
        component_name          = name
        interface_name          = name
        arg_name                = name
        procedure_entity_name   = name
        index_name              = name
        final_subroutine_name   = name
        block_data_name         = name
        if_construct_name       = name
        namelist_group_name     = name
        where_construct_name    = name
        forall_construct_name   = name
        select_construct_name   = name
        entity_name             = name
        array_name              = name
        case_construct_name     = name
        associate_name          = name
        associate_construct_name= name
        external_name           = name
        intrinsic_procedure_name= name

        ################## base terms ###################
        letter_spec             = letter (_0 "-" _0 letter )?
        name                    = letter ~"[_A-Z0-9]{{0,63}}"i
        label                   = ~"[0-9]{{1,5}}"
        exponent_letter         = ~"[ED]"i
        sign                    = "+" / "-"
        digit_string            = ~"[0-9]+"
        letter                  = ~"[A-Z]"i
        rep_char                = ~"{smapstr}[\d]+"

        ################## utilities ###################
        _CL                     = _C / _L
        _C                      = _0 CMT EOL
        _L                      = _0 EOL
        CMT                     = ~"{cmapstr}[\d]+"
        EOL                     = ~"[\r\n]"
        _1                      = ~"[ \t]+"
        _0                      = ~"[ \t]*"
    """.format(
        smapstr=utils.SMAPSTR,
        cmapstr=utils.CMAPSTR
    ) )

#comment = Regex(r'#[^\r\n]*', name='comment')

class FortParserVisitor(NodeVisitor):
    def generic_visit(self, node, visited_children):
        #import pdb; pdb.set_trace()
        if node.expr_name:
            print(node.expr_name, '---{}---'.format(node.text))

def main(preprocessed):
    #import pdb; pdb.set_trace()
    #tree = f2003_grammar.parse(open(os.path.join(utils.here, 'add.f90'), 'r').read())
    tree = f2003_grammar.parse('\n'.join(preprocessed['newlines']))
    FortParserVisitor().visit(tree)
    #print(tree)
