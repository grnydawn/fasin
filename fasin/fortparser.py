# -*- coding: utf-8 -*-
from __future__ import absolute_import, division, print_function, unicode_literals

import os, sys, re
from . import utils, parsergen

sys.setrecursionlimit(2000)

f2003_grammar_spec = r"""
    # Fortran2003 grammar specification excerpt from J3/04-007
    # __version__0_1_0
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
    block_data              = block_data_stmt specification_part? _CL*
                              end_block_data_stmt
    specification_part      = (_CL* use_stmt)*  (_CL* import_stmt)* implicit_part*
                              declaration_construct*
    implicit_part           = implicit_stmt / implicit_part_stmt*
    implicit_part_stmt      = implicit_stmt / parameter_stmt / format_stmt /
                              entry_stmt / _CL
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
    case_construct          = select_case_stmt (_CL* case_stmt block)* _CL*
                              end_select_stmt
    associate_construct     = associate_stmt _CL* block _CL* end_associate_stmt
    derived_type_def        = derived_type_stmt (_CL* type_param_def_stmt)*
                              (_CL* private_or_sequence)* component_part?
                              type_bound_procedure_part? _CL* end_type_stmt
    enum_def                = enum_def_stmt (_CL* enumerator_def_stmt)+ _CL*
                              end_enum_stmt

    ################## statement groups ###################
    do_block                = block
    block                   = (!continue_stmt execution_part_construct)*
    do_body                 = execution_part_construct*
    component_part          = (_CL* component_def_stmt)*
    type_bound_procedure_part = _CL* contains_stmt (_CL* binding_private_stmt)? _CL*
                              proc_binding_stmt (_CL* proc_binding_stmt)*
    interface_specification = interface_body / procedure_stmt
    interface_body          = (function_stmt specification_part? _CL* end_function_stmt) /
                              (subroutine_stmt specification_part? _CL* end_subroutine_stmt)
    end_do                  = continue_stmt / end_do_stmt
    private_or_sequence     = private_components_stmt / sequence_stmt

    ################## statements ###################
    program_stmt            = _L program_re _1 program_name _CL
    module_stmt             = _L module_re _1 module_name _CL
    interface_stmt          = (_L interface_re (_1 generic_spec)? _CL) /
                              (_L abstract_re _1 interface_re _CL)
    block_data_stmt         = _L block_re _0 data_re (_1 block_data_name)? _CL
    use_stmt                = (_L use_re !name ((_0 comma _0 module_nature)? _0 dcolons)? _0
                              module_name (_0 comma _0 rename_list)? _CL) /
                              (_L use_re !name ((_0 comma _0 module_nature)? _0 dcolons)? _0
                              module_name _0 comma _0 only_re _0 colon (_0 only_list)? _CL)
    implicit_stmt           = (_L implicit_re _1 implicit_spec_list _CL) /
                              (_L implicit_re _1 none_re _CL)
    function_stmt           = _L prefix? function_re _1 function_name _0
                              lparen _0 dummy_arg_name_list? _0 rparen (_0 suffix)? _CL
    contains_stmt           = _L contains_re _CL
    subroutine_stmt         = _L prefix? subroutine_re _1 subroutine_name (_0 lparen (_0
                              dummy_arg_list)? _0 rparen (_0 proc_language_binding_spec)?)? _CL
    enum_def_stmt           = _L enum_re _0 comma _0  bind_re _0 lparen _0 c_re _0 rparen _CL
    enumerator_def_stmt     = _L enumerator_re !name (_0 dcolons)? _0 enumerator_list _CL
    if_then_stmt            = _L (if_construct_name colon _0)? if_re _0 lparen _0
                              scalar_logical_expr _0 rparen _0 then_re _CL
    else_if_stmt            = _L else_re _0 if_re _0 lparen _0 scalar_logical_expr _0 rparen _0
                              then_re (_0 if_construct_name)? _CL
    else_stmt               = _L else_re (_1 if_construct_name)? _CL
    do_term_action_stmt     = action_stmt
    where_construct_stmt    = _L (where_construct_name _0 colon _0)? where_re _0 lparen _0
                              mask_expr _0 rparen _CL
    where_assignment_stmt   = assignment_stmt
    masked_elsewhere_stmt   = _L elsewhere_re _0 lparen _0 mask_expr _0 rparen
                              (_0 where_construct_name)? _CL
    elsewhere_stmt          = _L elsewhere_re (_0 where_construct_name)? _CL
    forall_construct_stmt   = _L (forall_construct_name _0 colon _0)? forall_re _0
                              forall_header _CL
    forall_assignment_stmt  = pointer_assignment_stmt / assignment_stmt
    select_type_stmt        = _L (select_construct_name _0 colon _0)? select_re _0 type_re
                              _0 lparen (_0 associate_name _0 points)? _0 selector rparen _CL
    select_case_stmt        = _L (case_construct_name _0 colon _0)? select_re _0 case_re
                              _0 lparen _0 case_expr _0 rparen _CL
    case_stmt               = _L case_re _1 case_selector (_0 case_construct_name)? _CL
    associate_stmt          = _L (associate_construct_name _0 colon _0)? associate_re _0
                              lparen _0 association_list _0 rparen _CL
    procedure_declaration_stmt= _L procedure_re _0 lparen (_0 proc_interface )? _0 rparen
                              ((_0 comma _0 proc_attr_spec )* _0 dcolons)? _0 proc_decl_list _CL
    import_stmt             = _L import_re !name ((_0 dcolons)? _0 import_name_list )? _CL
    access_stmt             = _L access_spec !name ((_0 dcolons)? _0 access_id_list )? _CL
    allocatable_stmt        = _L allocatable_re !name (_0 dcolons )? _0 object_name
                              (_0 lparen _0 deferred_shape_spec_list _0 rparen)? (_0 comma _0
                              object_name (_0 lparen _0 deferred_shape_spec_list _0 rparen)?)* _CL
    asynchronous_stmt       = _L asynchronous_re !name (_0 dcolons)? _0 object_name_list _CL
    bind_stmt               = _L language_binding_spec !name (_0 dcolons)? _0
                              bind_entity_list _CL
    common_stmt             = _L common_re (_1 slash (_0 common_block_name )? _0 slash)? _0
                              common_block_object_list ((_0 comma )? _0 slash
                              (_0 common_block_name )? _0 slash _0 common_block_object_list)* _CL
    data_stmt               = _L data_re _1 data_stmt_set ( (_0 comma)? _0 data_stmt_set )*
    dimension_stmt          = _L dimension_re !name (_0 dcolons)? _0 array_name _0 lparen
                              _0 array_spec _0 rparen (_0 comma _0 array_name _0 lparen _0
                              array_spec _0 rparen)* _CL
    equivalence_stmt        = _L equivalence_re _0 equivalence_set_list _CL
    external_stmt           = _L external_re !name (_0 dcolons)? _0 external_name_list _CL
    intent_stmt             = _L intent_re _0 lparen _0 intent_spec _0 rparen (_0 dcolons)?
                              _0 dummy_arg_name_list _CL
    intrinsic_stmt          = _L intrinsic_re !name (_0 dcolons)? _0
                              intrinsic_procedure_name_list _CL
    namelist_stmt           = _L namelist_re _0 slash _0 namelist_group_name _0 slash _0
                              namelist_group_object_list ((_0 comma )? _0 slash _0
                              namelist_group_name _0 slash _0 namelist_group_object_list)* _CL
    optional_stmt           = _L optional_re !name (_0 dcolons)? _0 dummy_arg_name_list _CL
    pointer_stmt            = _L pointer_re !name (_0 dcolons)? _0 pointer_decl_list _CL
    protected_stmt          = _L protected_re !name (_0 dcolons)? _0 entity_name_list _CL
    save_stmt               = _L save_re !name ((_0 dcolons)? _0 saved_entity_list)? _CL
    target_stmt             = _L target_re !name (_0 dcolons)? _0 object_name (_0 lparen _0
                              array_spec _0 rparen)? (_0 comma _0 object_name
                              (_0 lparen _0 array_spec _0 rparen)?)* _CL
    volatile_stmt           = _L volatile_re !name (_0 dcolons)? _0 object_name_list _CL
    value_stmt              = _L value_re !name (_0 dcolons)? _0 dummy_arg_name_list _CL
    do_stmt                 = label_do_stmt / nonlabel_do_stmt
    nonlabel_do_stmt        = _L (do_construct_name _0 colon _0)? do_re (_1 loop_control)? _CL
    label_do_stmt           = _L (do_construct_name _0 colon _0)? do_re _1 label (_1 loop_control)? _CL
    allocate_stmt           = _L allocate_re _0 lparen (_0 type_spec _0 dcolons)? _0 allocation_list
                              (_0 comma _0 alloc_opt_list)? _0 rparen _CL
    assignment_stmt         = _L variable _0 equal _0 expr _CL
    backspace_stmt          = (_L backspace_re _1 file_unit_number _CL) /
                              (_L backspace_re _0 lparen _0 position_spec_list _0 rparen _CL)
    call_stmt               = _L call_re _1 procedure_designator (_0 lparen
                              (_0 actual_arg_spec_list)?  _0 rparen)? _CL
    close_stmt              = _L close_re _0 lparen _0 close_spec_list _0 rparen _CL
    continue_stmt           = _L continue_re _CL
    cycle_stmt              = _L cycle_re (_1 do_construct_name )? _CL
    deallocate_stmt         = _L deallocate_re _0 lparen _0 allocate_object_list (_0 comma _0
                              dealloc_opt_list)? _0 rparen _CL
    exit_stmt               = _L exit_re (_0 do_construct_name)? _CL
    flush_stmt              = (_L flush_re _0 file_unit_number _CL) / (_L flush_re _0 lparen _0
                              flush_spec_list _0 rparen _CL)
    forall_stmt             = _L forall_re _0 forall_header _0 forall_assignment_stmt _CL
    goto_stmt               = _L go_re _0 to_re _1 label _CL
    if_stmt                 = _L if_re _0 lparen _0 scalar_logical_expr _0 rparen action_stmt # DO NOT put _CL
    inquire_stmt            = (_L inquire_re _0 lparen _0 inquire_spec_list _0 rparen _CL) /
                              (_L inquire_re _0 lparen _0 iolength_re _0 equal _0 scalar_int_variable _0 rparen
                              _0 output_item_list _CL)
    nullify_stmt            = _L nullify_re _0 lparen _0 pointer_object_list _0 rparen _CL
    open_stmt               = _L open_re _0 lparen _0 connect_spec_list _0 rparen _CL
    pointer_assignment_stmt = (_L proc_pointer_object _0 points _0 proc_target _CL) /
                              (_L data_pointer_object (_0 lparen _0 bounds_spec_list _0 rparen)? _0
                              points _0 data_target _CL) / (_L data_pointer_object _0 lparen _0
                              bounds_remapping_list _0 rparen _0 points _0 data_target _CL)
    print_stmt              = _L print_re _1 format (_0 comma _0 output_item_list)? _CL
    read_stmt               = (_L read_re _0 lparen _0 io_control_spec_list _0 rparen (_0 input_item_list)?
                              _CL) / (_L read_re _1 format (_0 comma _0 input_item_list)? _CL)
    return_stmt             = _L return_re (_0 scalar_int_expr)? _CL
    rewind_stmt             = (_L rewind_re _0 file_unit_number _CL) /
                              (_L rewind_re _0 lparen _0 position_spec_list _0 rparen _CL)
    endfile_stmt            = (_L endfile_re _0 file_unit_number _CL) /
                              (_L endfile_re _0 lparen _0 position_spec_list _0 rparen  _CL)
    stop_stmt               = _L stop_re (_0 stop_code)? _CL
    wait_stmt               = _L wait_re _0 lparen _0 wait_spec_list _0 rparen _CL
    where_stmt              = _L where_re _0 lparen _0 mask_expr _0 rparen _0 where_assignment_stmt _CL
    write_stmt              = _L write_re _0 lparen _0 io_control_spec_list _0 rparen
                              (_0 output_item_list)? _CL
    arithmetic_if_stmt      = _L if_re _0 lparen _0 scalar_numeric_expr _0 rparen _0 label _0 comma
                              _0 label _0 comma _0 label _CL
    computed_goto_stmt      = _L go_re _0 to_re _0 lparen _0 label_list _0 rparen (_0 comma)? _0
                              scalar_int_expr _CL
    stmt_function_stmt      = _L function_name _0 lparen (_0 dummy_arg_name_list )? _0 rparen _0 equal _0
                              scalar_expr _CL
    type_declaration_stmt   = _L declaration_type_spec !entity_decl ((_0 comma _0 attr_spec)* _0 dcolons)? _0
                              entity_decl_list _CL
    parameter_stmt          = _L parameter_re _0 lparen _0 named_constant_def_list _0 rparen _CL
    format_stmt             = _L format_re _0 format_specification _CL
    entry_stmt              = _L entry_re _0 entry_name (_0 lparen (_0 dummy_arg_list)? _0 rparen
                              (_0 suffix)?)? _CL
    procedure_stmt          = _L (module_re _1)? procedure_re _1 procedure_name_list _CL
    private_components_stmt = _L private_re _CL
    sequence_stmt           = _L sequence_re _CL
    component_def_stmt      = data_component_def_stmt / proc_component_def_stmt
    data_component_def_stmt = _L declaration_type_spec !component_decl ((_0 comma _0 component_attr_spec_list)?
                              _0 dcolons)? _0 component_decl_list _CL
    proc_component_def_stmt = _L procedure_re _0 lparen (_0 proc_interface)? _0 rparen _0 comma _0
                              proc_component_attr_spec_list _0 dcolons _0 proc_decl_list _CL
    derived_type_stmt       = _L type_re !name ((_0 comma _0 type_attr_spec_list)? _0 dcolons)?
                              _0 type_name (_0 lparen _0 type_param_name_list _0 rparen)? _CL
    type_param_def_stmt     = _L integer_re (_0 kind_selector)? _0 comma _0 type_param_attr_spec
                              _0 dcolons _0 type_param_decl_list _CL
    binding_private_stmt    = _L private_re _CL
    proc_binding_stmt       = (_L specific_binding _CL) / (_L generic_binding _CL) /
                              (_L final_binding _CL)
    type_guard_stmt         = (_L type_re _1 is_re _0 lparen _0 type_spec _0 rparen
                              (_0 select_construct_name)? _CL) / (_L class_re _1 is_re _0 lparen _0
                              type_spec _0 rparen (_0 select_construct_name)? _CL) /
                              (_L class_re _1 default_re (_0 select_construct_name )? _CL)


    ################## end statements ###################
    end_program_stmt        = _L end_re (_0 program_re (_1 program_name)?)? _CL
    end_module_stmt         = _L end_re (_0 module_re (_1 module_name)?)? _CL
    end_block_data_stmt     = _L end_re (_0 block_re _0 data_re (_1 block_data_name)?)? _CL
    end_function_stmt       = _L end_re (_0 function_re (_1 function_name)?)? _CL
    end_subroutine_stmt     = _L end_re (_0 subroutine_re (_1 subroutine_name)?)? _CL
    end_interface_stmt      = _L end_re _0 interface_re (_1 generic_spec)? _CL
    end_do_stmt             = _L end_re _0 do_re (_1 do_construct_name)? _CL
    end_if_stmt             = _L end_re _0 if_re (_1 if_construct_name)? _CL
    end_type_stmt           = _L end_re _0 type_re (_1 type_name)? _CL
    end_where_stmt          = _L end_re _0 where_re (_1 where_construct_name)? _CL
    end_forall_stmt         = _L end_re _0 forall_re (_1 forall_construct_name)? _CL
    end_select_stmt         = _L end_re _0 select_re (_1 select_construct_name)? _CL
    end_associate_stmt      = _L end_re _0 associate_re (_1 associate_construct_name)? _CL
    end_enum_stmt           = _L end_re _0 enum_re _CL
    end_select_type_stmt    = end_select_stmt

    ################## expressions and operands ###################
    file_name_expr          = scalar_default_char_expr
    case_expr               = scalar_int_expr / scalar_char_expr / scalar_logical_expr
    scalar_logical_initialization_expr = logical_initialization_expr
    scalar_char_initialization_expr = char_initialization_expr
    logical_initialization_expr= logical_expr
    char_initialization_expr= char_expr
    scalar_numeric_expr     = numeric_expr
    scalar_char_expr        = char_expr
    upper_bound_expr        = scalar_int_expr
    lower_bound_expr        = scalar_int_expr
    scalar_int_expr         = int_expr
    scalar_logical_expr     = logical_expr
    scalar_mask_expr        = mask_expr
    mask_expr               = logical_expr
    specification_expr      = scalar_int_expr
    scalar_default_char_expr= default_char_expr
    scalar_expr             = expr
    source_expr             = expr
    default_char_expr       = expr
    char_expr               = expr
    logical_expr            = expr
    numeric_expr            = expr
    int_expr                = expr
    initialization_expr     = expr
    scalar_int_initialization_expr = expr

    expr                    = level_5_expr expr_opt
    expr_opt                = (_0 defined_binary_op _0 level_5_expr expr_opt)?
    level_5_expr            = equiv_operand level_5_expr_opt
    level_5_expr_opt        = (_0 equiv_op _0 equiv_operand level_5_expr_opt)?
    equiv_operand           = or_operand equiv_operand_opt
    equiv_operand_opt       = (_0 or_op _0 or_operand equiv_operand_opt)?
    or_operand              = and_operand or_operand_opt
    or_operand_opt          = (_0 and_op _0 and_operand or_operand_opt)?
    and_operand             = (not_op _0)? level_4_expr
    level_4_expr            = (level_3_expr _0 rel_op _0)? level_3_expr
    level_3_expr            = level_2_expr level_3_expr_opt
    level_3_expr_opt        = (_0 concat_op _0 level_2_expr level_3_expr_opt)?
    level_2_expr            = (add_op _0 add_operand level_2_expr_opt) /
                              (add_operand level_2_expr_opt)
    level_2_expr_opt        = (_0 add_op _0 add_operand level_2_expr_opt)?
    add_operand             = mult_operand add_operand_opt
    add_operand_opt         = (_0 mult_op _0 mult_operand add_operand_opt)?
    mult_operand            = level_1_expr (_0 power_op _0 mult_operand)?
    level_1_expr            = (defined_unary_op _0)? primary
    primary                 = boz_literal_constant / array_constructor / structure_constructor /
                              function_reference / type_param_inquiry / (lparen _0 expr _0 rparen) /
                              designator / constant / type_param_name


    ################## specs ###################
    proc_attr_spec          = save_re / pointer_re / (intent_re _0 lparen _0 intent_spec _0 rparen) /
                              access_spec / proc_language_binding_spec
    bounds_spec             = lower_bound_expr _0 colon
    forall_triplet_spec     = index_name _0 equal _0 subscript _0 colon _0 subscript
                              (_0 colon _0 stride)?
    prefix_spec             = recursive_re / pure_re / elemental_re / declaration_type_spec
    proc_language_binding_spec = language_binding_spec
    type_attr_spec          = access_spec / (extends_re _0 lparen _0 parent_type_name _0 rparen) /
                              abstract_re / (bind_re _0 lparen _0 c_re _0 rparen)
    type_param_attr_spec    = kind_re /len_re
    type_param_decl         = type_param_name (_0 equal _0 scalar_int_initialization_expr )?
    component_attr_spec     = pointer_re / (dimension_re _0 lparen _0 component_array_spec _0 rparen) /
                              allocatable_re / access_spec
    component_array_spec    = explicit_shape_spec_list / deferred_shape_spec_list
    proc_component_attr_spec= pointer_re / (pass_re ( _0 lparen _0 arg_name _0 rparen)?) /
                              nopass_re / access_spec
    generic_spec            = (assignment_re _0 lparen _0 equal _0 rparen) /
                              (operator_re _0 lparen _0 defined_operator _0 rparen) /
                              dtio_generic_spec / generic_name
    dtio_generic_spec       = (read_re _0 lparen _0 formatted_re _0 rparen) /
                              (read_re _0 lparen _0 unformatted_re _0 rparen) /
                              (write_re _0 lparen _0 formatted_re _0 rparen) /
                              (write_re _0 lparen _0 unformatted_re _0 rparen)
    inquire_spec            = (access_re _0 equal _0 scalar_default_char_variable) /
                              (action_re _0 equal _0 scalar_default_char_variable) /
                              (asynchronous_re _0 equal _0 scalar_default_char_variable) /
                              (blank_re _0 equal _0 scalar_default_char_variable) /
                              (decimal_re _0 equal _0 scalar_default_char_variable) /
                              (delim_re _0 equal _0 scalar_default_char_variable) /
                              (direct_re _0 equal _0 scalar_default_char_variable) /
                              (encoding_re _0 equal _0 scalar_default_char_variable) /
                              err_spec_cr / iostat_spec_cr  / size_spec_cr /
                              (exist_re _0 equal _0 scalar_default_logical_variable) /
                              (form_re _0 equal _0 scalar_default_char_variable) /
                              (formatted_re _0 equal _0 scalar_default_char_variable) /
                              (id_re _0 equal _0 scalar_int_expr) / iomsg_spec_cr  /
                              (name_re _0 equal _0 scalar_default_char_variable) /
                              (named_re _0 equal _0 scalar_default_logical_variable) /
                              (nextrec_re _0 equal _0 scalar_int_variable) /
                              (number_re _0 equal _0 scalar_int_variable) /
                              (opened_re _0 equal _0 scalar_default_logical_variable) /
                              (pad_re _0 equal _0 scalar_default_char_variable) /
                              (pending_re _0 equal _0 scalar_default_logical_variable) /
                              (pos_re _0 equal _0 scalar_int_variable) /
                              (position_re _0 equal _0 scalar_default_char_variable) /
                              (read_re _0 equal _0 scalar_default_char_variable) /
                              (readwrite_re _0 equal _0 scalar_default_char_variable) /
                              (recl_re _0 equal _0 scalar_int_variable) /
                              (round_re _0 equal _0 scalar_default_char_variable) /
                              (sequential_re _0 equal _0 scalar_default_char_variable) /
                              (sign_re _0 equal _0 scalar_default_char_variable) /
                              (stream_re _0 equal _0 scalar_default_char_variable) /
                              (unformatted_re _0 equal _0 scalar_default_char_variable) /
                              (write_re _0 equal _0 scalar_default_char_variable) /
                              file_spec_cr / unit_spec_cr

    io_control_spec         = (advance_re _0 equal _0 scalar_default_char_expr) /
                              (asynchronous_re _0 equal _0 scalar_char_initialization_expr) /
                              blank_spec_cr / decimal_spec_cr / delim_spec_cr /
                              end_spec_cr / eor_spec_cr / err_spec_cr /
                              (id_re _0 equal _0 scalar_int_variable) /
                              iomsg_spec_cr / iostat_spec_cr / pad_spec_cr /
                              (pos_re _0 equal _0 scalar_int_expr) /
                              (rec_re _0 equal _0 scalar_int_expr) /
                              round_spec_cr / sign_spec_cr / size_spec_cr /
                              ((unit_re _0 equal _0)? io_unit) / ((fmt_re _0 equal _0)? format) /
                              ((nml_re _0 equal _0)? namelist_group_name)

    array_spec              = explicit_shape_spec_list / assumed_shape_spec_list /
                              deferred_shape_spec_list / assumed_size_spec
    attr_spec               = access_spec / allocatable_re / asynchronous_re /
                              (dimension_re _0 lparen _0 array_spec _0 rparen) / external_re /
                              (intent_re _0 lparen _0 intent_spec _0 rparen) / intrinsic_re /
                              language_binding_spec / optional_re / parameter_re /
                              pointer_re / protected_re / save_re/ target_re /
                              value_re / volatile_re
    access_spec             = public_re / private_re
    explicit_shape_spec     = (lower_bound _0 colon _0)? upper_bound
    ac_spec                 = (type_spec _0 dcolons) / ((type_spec !name _0 dcolons)? ac_value_list)
    type_spec               = intrinsic_type_spec / derived_type_spec
    derived_type_spec       = type_name (_0 lparen _0 type_param_spec_list _0 rparen)?
    type_param_spec         = (keyword _0 equal _0)? type_param_value
    assumed_shape_spec      = (lower_bound _0)? colon
    deferred_shape_spec     = colon
    assumed_size_spec       = (explicit_shape_spec_list _0 comma)? (_0 lower_bound _0 colon _0)? star
    intent_spec             = (in_re _0 out_re) / in_re / out_re
    language_binding_spec   = bind_re _0 lparen _0 c_re (_0 comma _0 name_re _0 equal _0
                              scalar_char_initialization_expr )? _0 rparen
    component_spec          = (keyword _0 equal _0)? component_data_source
    actual_arg_spec         = (keyword _0 equal _0)? actual_arg
    alt_return_spec         = star _0 label
    declaration_type_spec   = (type_re _0 lparen _0 derived_type_spec _0 rparen) /
                              (class_re _0 lparen _0 derived_type_spec _0 rparen) /
                              (class_re _0 lparen _0 star _0 rparen) / intrinsic_type_spec
    # NOTE: length_selector is added ( not in spec. )
    intrinsic_type_spec     = (integer_re (_0 (kind_selector / length_selector))?) /
                              (real_re (_0 (kind_selector / length_selector))?) /
                              (double_re _0 precision_re) /
                              (complex_re (_0 (kind_selector / length_selector))?) /
                              (character_re (_0 char_selector)?) /
                              (logical_re (_0 (kind_selector / length_selector))?)
    implicit_spec           = declaration_type_spec _0 lparen _0 letter_spec_list _0 rparen
    position_spec           = iomsg_spec_cr / iostat_spec_cr /
                              err_spec_cr / unit_spec_cr
    wait_spec               = end_spec_cr / eor_spec_cr / err_spec_cr /
                              (id_re _0 equal _0 scalar_int_expr) / iomsg_spec_cr /
                              iostat_spec_cr / unit_spec_cr
    flush_spec              = iostat_spec_cr / iomsg_spec_cr / err_spec_cr /
                              unit_spec_cr
    close_spec              = iostat_spec_cr / iomsg_spec_cr / err_spec_cr /
                              (status_re _0 equal _0 scalar_default_char_variable) /
                              unit_spec_cr
    allocate_shape_spec     = (lower_bound_expr _0 colon _0)? upper_bound_expr

    connect_spec            = access_spec_cr / action_spec_cr / asynchronous_spec_cr /
                              blank_spec_cr / decimal_spec_cr / delim_spec_cr /
                              (encoding_re _0 equal _0 scalar_default_char_expr) /
                              err_spec_cr / file_spec_cr /
                              (form_re _0 equal _0 scalar_default_char_expr) /
                              iomsg_spec_cr / iostat_spec_cr / pad_spec_cr /
                              (position_re _0 equal _0 scalar_default_char_expr) /
                              (recl_re _0 equal _0 scalar_int_expr) /
                              round_spec_cr / sign_spec_cr / newerspec_convert_spec_cr /
                              (status_re _0 equal _0 scalar_default_char_expr) /
                              unit_spec_cr

    ################## sub-expressions ###################
    # TODO: refine format specification
    format_specification    = lparen (_0 format_item_list)? _0 rparen

    format_item             = ~"{fmapstr}" / p_combined / char_string_edit_desc / control_edit_desc /
                              ((!p_re !x_re !slash r)? lparen _0 format_item_list _0 rparen) /
                              ((!p_re !x_re !slash r)? data_edit_desc)
    p_combined             = (!x_re !slash !lparen r)? p_re _0 ((f_re w dot d) /
                              (e_re w dot d (e_re e)?) / (en_re w dot d (e_re e)?) /
                              (es_re w dot d (e_re e)?) / (g_re w dot d (e_re e)?) / (d_re w dot d))

    data_edit_desc          = (i_re w (dot m)?) / (b_re w (dot m)?) /
                              (o_re w (dot m)?) / (z_re w (dot m)?) /
                              (f_re w dot d) / (e_re w dot d (e_re e)?) /
                              (en_re w dot d (e_re e)?) / (es_re w dot d (e_re e)?) /
                              (g_re w dot d (e_re e)?) / (l_re w) / (a_re w?) /
                              (d_re w dot d) / (dt_re (char_literal_constant)?
                              (lparen _0 v_list _0 rparen)?)
    r                       = int_literal_constant
    w                       = int_literal_constant
    m                       = int_literal_constant
    d                       = int_literal_constant
    e                       = int_literal_constant
    n                       = int_literal_constant
    v                       = signed_int_literal_constant
    k                       = signed_int_literal_constant
    control_edit_desc       = position_edit_desc / ((!p_re !x_re !lparen r)? slash) / colon /
                              sign_edit_desc / (k p_re) / blank_interp_edit_desc /
                              round_edit_desc / decimal_edit_desc
    position_edit_desc      = (t_re n) / (tl_re n) / (tr_re n) / (n x_re)
    sign_edit_desc          = ss_re / sp_re / s_re
    blank_interp_edit_desc  = bn_re / bz_re
    round_edit_desc         = ru_re / rd_re / rz_re / rn_re / rc_re / rp_re
    decimal_edit_desc       = dc_re / dp_re
    char_string_edit_desc   = char_literal_constant

    access_spec_cr          = access_re _0 equal _0 scalar_default_char_expr
    newerspec_convert_spec_cr= convert_re _0 equal _0 scalar_default_char_expr
    action_spec_cr          = action_re _0 equal _0 scalar_default_char_expr
    asynchronous_spec_cr    = asynchronous_re _0 equal _0 scalar_default_char_expr
    unit_spec_cr            = (unit_re _0 equal _0)? file_unit_number
    file_spec_cr            = file_re _0 equal _0 file_name_expr
    blank_spec_cr           = blank_re _0 equal _0 scalar_default_char_expr
    delim_spec_cr           = delim_re _0 equal _0 scalar_default_char_expr
    decimal_spec_cr         = decimal_re _0 equal _0 scalar_default_char_expr
    err_spec_cr             = err_re _0 equal _0 label
    size_spec_cr            = size_re _0 equal _0 scalar_int_variable
    sign_spec_cr            = sign_re _0 equal _0 scalar_default_char_expr
    round_spec_cr           = round_re _0 equal _0 scalar_default_char_expr
    pad_spec_cr             = pad_re _0 equal _0 scalar_default_char_expr
    iostat_spec_cr          = iostat_re _0 equal _0 scalar_int_variable
    iomsg_spec_cr           = iomsg_re _0 equal _0 iomsg_variable
    eor_spec_cr             = eor_re _0 equal _0 label
    end_spec_cr             = end_re _0 equal _0 label

    namelist_group_object   = variable_name
    pointer_decl            = (object_name (_0 lparen _0 deferred_shape_spec_list _0 rparen)?) /
                              proc_entity_name
    selector                = expr / variable
    bind_entity             = (slash _0 common_block_name _0 slash) / entity_name
    access_id               = generic_spec / use_name
    data_stmt_set           = data_stmt_object_list _0 slash _0 data_stmt_value_list _0 slash
    data_stmt_object        = data_implied_do / variable
    data_implied_do         = lparen _0 data_i_do_object_list _0 comma _0 data_i_do_variable _0
                              equal _0 scalar_int_expr _0 comma _0 scalar_int_expr (_0 comma _0
                              scalar_int_expr)? _0 rparen
    data_i_do_object        = array_element / scalar_structure_component / data_implied_do
    data_stmt_value         = (data_stmt_repeat _0 star _0)? data_stmt_constant
    data_stmt_repeat        = scalar_int_constant_subobject / scalar_int_constant
    scalar_int_constant_subobject = int_constant_subobject
    scalar_constant_subobject = constant_subobject
    int_constant_subobject  = constant_subobject
    constant_subobject      = designator
    data_stmt_constant      = structure_constructor / null_init/ signed_real_literal_constant /
                              signed_int_literal_constant / scalar_constant_subobject / scalar_constant
    common_block_object     = (variable_name (_0 lparen _0 explicit_shape_spec_list _0 rparen)?) /
                              proc_pointer_name
    data_pointer_object     = (variable _0 percent _0 data_pointer_component_name) / variable_name
    proc_pointer_object     = proc_component_ref / proc_pointer_name
    bounds_remapping        = lower_bound_expr _0 colon _0 upper_bound_expr
    forall_header           = lparen _0 forall_triplet_spec_list (_0 comma _0 scalar_mask_expr)? _0 rparen
    association             = associate_name _0 points _0 selector
    case_selector           = default_re / (lparen _0 case_value_range_list _0 rparen)
    case_value_range        = case_value / (case_value _0 colon) / (colon _0 case_value) /
                              (case_value _0 colon _0 case_value)
    case_value              = scalar_int_initialization_expr / scalar_char_initialization_expr /
                              scalar_logical_initialization_expr
    designator              = array_section / array_element / structure_component /
                              substring / object_name
    module_nature           = non_intrinsic_re / intrinsic_re
    rename                  = (operator_re _0 lparen _0 local_defined_operator _0 rparen _0 points _0
                              operator_re _0 lparen _0 use_defined_operator _0 rparen) /
                              (local_name _0 points _0 use_name)
    only                    = rename / generic_spec / only_use_name
    prefix                  = (prefix_spec _1)+
    suffix                  = (proc_language_binding_spec (_1 result_re _0 lparen _0
                              result_name _0 rparen)?) / (result_re _0 lparen _0
                              result_name _0 rparen (_0 proc_language_binding_spec)?)
    dummy_arg               = star / dummy_arg_name
    component_decl          = component_name (_0 lparen _0 component_array_spec _0 rparen)?
                              (_0 star _0 char_length)? (_0 component_initialization)?
    component_initialization= initialization
    proc_interface          = (interface_name !(_0 lparen)) / declaration_type_spec
    proc_decl               = procedure_entity_name (_0 points _0 null_init)?
    specific_binding        = procedure_re !name (_0 lparen _0 interface_name _0 rparen)? ((_0 comma _0
                              binding_attr_list)? _0 dcolons _0)? binding_name (_0 points _0 procedure_name)?
    binding_attr            = (pass_re (_0 lparen _0 arg_name _0 rparen)?) / nopass_re /
                              non_overridable_re / deferred_re / access_spec
    generic_binding         = generic_re (_0 comma _0 access_spec _0)? dcolons _0
                              generic_spec _0 points _0 binding_name_list
    final_binding           = final_re !name (_0 dcolons)? _0 final_subroutine_name_list
    enumerator              = named_constant (_0 equal _0 scalar_int_initialization_expr)?
    saved_entity            = (slash _0 common_block_name _0 slash ) / proc_pointer_name /
                              object_name
    array_constructor       = (lslash _0 ac_spec _0 rslash) / (lbracket _0 ac_spec _0 rbracket)
    structure_constructor   = derived_type_spec _0 lparen (_0 component_spec_list)? _0 rparen
    loop_control            = ((comma _0)? do_variable _0 equal _0 scalar_int_expr _0
                              comma _0 scalar_int_expr (_0 comma _0 scalar_int_expr )?) /
                              ((comma _0)? while_re _0 lparen _0 scalar_logical_expr _0 rparen)
    io_unit                 = star / file_unit_number / internal_file_variable
    file_unit_number        = scalar_int_expr
    format                  = star / label / format_specification
    stop_code               = scalar_char_constant / fivedigit
    output_item             = io_implied_do / expr
    input_item              = io_implied_do / variable
    io_implied_do           = lparen _0 io_implied_do_object_list _0 comma _0 io_implied_do_control
                              _0 rparen
    io_implied_do_object    = input_item / output_item
    io_implied_do_control   = do_variable _0 equal _0 scalar_int_expr _0 comma _0 scalar_int_expr
                              (_0 comma _0 scalar_int_expr )?
    entity_decl             = (object_name (_0 lparen _0 array_spec _0 rparen)? (_0 star _0 char_length)?
                              (_0 initialization)?) / (function_name (_0 star _0 char_length)?)
    lower_bound             = specification_expr
    upper_bound             = specification_expr
    initialization          = (equal _0 initialization_expr) / (points _0 null_init)
    null_init               = function_reference
    kind_param              = digit_string / scalar_int_constant_name
    significand             = (digit_string dot digit_string?) / (dot digit_string)
    exponent                = signed_digit_string
    signed_digit_string     = (sign _0)? digit_string
    real_part               = signed_int_literal_constant / signed_real_literal_constant /
                              named_constant
    imag_part               = signed_int_literal_constant / signed_real_literal_constant /
                              named_constant
    array_element           = data_ref
    data_ref                = part_ref (_0 percent _0 part_ref)*
    part_ref                = part_name (_0 lparen _0 section_subscript_list _0 rparen)?
    section_subscript       = subscript_triplet/ vector_subscript / subscript
    subscript               = scalar_int_expr
    subscript_triplet       = (subscript _0)? colon (_0 subscript)? (_0 colon _0 stride)?
    stride                  = scalar_int_expr
    vector_subscript        = int_expr
    array_section           = data_ref (_0 lparen _0 substring_range _0 rparen)?
    substring_range         = (scalar_int_expr _0)? colon (_0 scalar_int_expr)?
    structure_component     = data_ref
    substring               = parent_string _0 lparen _0 substring_range _0 rparen
    parent_string           = array_element / scalar_structure_component /
                              scalar_constant / scalar_variable_name
    scalar_structure_component = structure_component
    type_param_value        = star / colon / scalar_int_expr
    ac_value                = ac_implied_do / expr
    ac_implied_do           = lparen _0 ac_value_list _0 comma _0 ac_implied_do_control
                              _0 rparen
    ac_implied_do_control   = ac_do_variable _0 equal _0 scalar_int_expr _0 comma _0
                              scalar_int_expr (_0 comma _0 scalar_int_expr)?
    component_data_source   = data_target / proc_target / expr
    data_target             = variable / expr
    proc_target             = proc_component_ref / procedure_name / expr
    proc_component_ref      = variable _0 percent _0 procedure_component_name
    function_reference      = procedure_designator _0 lparen (_0 actual_arg_spec_list)?
                              _0 rparen
    procedure_designator    = proc_component_ref / (_0 data_ref _0 percent _0
                              binding_name) / procedure_name
    actual_arg              = expr / variable / procedure_name / proc_component_ref /
                              alt_return_spec
#    actual_arg              = proc_component_ref / alt_return_spec / variable /
#                              procedure_name / expr
    type_param_inquiry      = designator _0 percent _0 type_param_name
    char_length             = (lparen _0 type_param_value _0 rparen) /
                              scalar_int_literal_constant
    kind_selector           = lparen (_0 kind_re _0 equal)? _0
                              scalar_int_initialization_expr _0 rparen
    char_selector           = length_selector /
                              (lparen _0 len_re _0 equal _0 type_param_value _0 comma
                              _0 kind_re _0 equal _0 scalar_int_initialization_expr
                              _0 rparen) / (lparen _0 type_param_value _0 comma
                              (_0 kind_re _0 equal)? _0 scalar_int_initialization_expr
                              _0 rparen) / (lparen _0 kind_re _0 equal _0
                              scalar_int_initialization_expr (_0 comma _0 len_re _0 equal _0
                              type_param_value)? _0 rparen)
    length_selector         = (lparen (_0 len_re equal)? _0 type_param_value _0 rparen) /
                              (star _0 char_length (_0 comma)?)
    equivalence_set         = lparen _0 equivalence_object _0 comma _0
                              equivalence_object_list _0 rparen
    equivalence_object      = substring / array_element / variable_name
    named_constant_def      = named_constant _0 equal _0 initialization_expr
    pointer_object          = structure_component / variable_name / proc_pointer_name
    allocate_object         = structure_component / variable_name
    dealloc_opt             = (stat_re _0 equal _0 stat_variable) /
                              (errmsg_re _0 equal _0 errmsg_variable)
    allocation              = allocate_object (_0 lparen _0 allocate_shape_spec_list _0 rparen)?
    alloc_opt               = (source_re _0 equal _0 source_expr) /
                              (stat_re _0 equal _0 stat_variable) /
                              (errmsg_re _0 equal _0 errmsg_variable)

    ################## operators ###################
    defined_operator        = defined_unary_op / defined_binary_op / extended_intrinsic_op
    local_defined_operator  = defined_unary_op / defined_binary_op
    use_defined_operator    = defined_unary_op / defined_binary_op
    extended_intrinsic_op   = intrinsic_operator
    defined_unary_op        = defined_unary_binary_op
    defined_binary_op       = defined_unary_binary_op
    defined_unary_binary_op = !equiv_op !or_op !and_op !not_op !concat_op !add_op !mult_op
                              !rel_op !true_re !false_re dot letter+ dot
    intrinsic_operator      = power_op / mult_op / add_op / concat_op / rel_op / not_op /
                              and_op / or_op / equiv_op
    power_op                = "**"
    mult_op                 = star / slash
    add_op                  = plus / minus
    concat_op               = "//"
    rel_op                  = eq_re / ne_re / lt_re / le_re / gt_re / ge_re /
                              dequal / notequal / lessequal / greaterequal / langle / rangle
    not_op                  = ~"\.NOT\."i
    and_op                  = ~"\.AND\."i
    or_op                   = ~"\.OR\."i
    equiv_op                = neqv_re / eqv_re

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
    int_literal_constant    = digit_string (underscore kind_param)?
    real_literal_constant   = (significand (exponent_letter exponent)?
                              (underscore kind_param)?) /
                              (digit_string exponent_letter exponent (underscore kind_param)?)
    complex_literal_constant= lparen _0 real_part _0 comma _0 imag_part _0 rparen
    logical_literal_constant= (true_re (underscore kind_param)?) /
                              (false_re (underscore kind_param)?)
    char_literal_constant   = (kind_param underscore)? rep_char
    boz_literal_constant    = binary_constant / octal_constant / hex_constant
    binary_constant         = b_re rep_char
    octal_constant          = o_re rep_char
    hex_constant            = z_re rep_char

    scalar_char_constant    = rep_char


    ################## lists ###################
    intrinsic_procedure_name_list= intrinsic_procedure_name
                              (_0 comma _0 intrinsic_procedure_name)*
    proc_component_attr_spec_list= proc_component_attr_spec
                              (_0 comma _0 proc_component_attr_spec)*
    final_subroutine_name_list = final_subroutine_name
                              (_0 comma _0 final_subroutine_name)*
    namelist_group_object_list= namelist_group_object
                              (_0 comma _0 namelist_group_object)*
    io_implied_do_object_list= io_implied_do_object
                              (_0 comma _0 io_implied_do_object)*
    common_block_object_list= common_block_object (_0 comma _0 common_block_object)*
    forall_triplet_spec_list= forall_triplet_spec (_0 comma _0 forall_triplet_spec)*
    component_attr_spec_list= component_attr_spec (_0 comma _0 component_attr_spec)*
    allocate_shape_spec_list= allocate_shape_spec (_0 comma _0 allocate_shape_spec)*
    deferred_shape_spec_list= deferred_shape_spec (_0 comma _0 deferred_shape_spec)*
    explicit_shape_spec_list= explicit_shape_spec (_0 comma _0 explicit_shape_spec)*
    named_constant_def_list = named_constant_def (_0 comma _0 named_constant_def)*
    assumed_shape_spec_list = assumed_shape_spec (_0 comma _0 assumed_shape_spec)*
    equivalence_object_list = equivalence_object (_0 comma _0 equivalence_object)*
    section_subscript_list  = section_subscript (_0 comma _0 section_subscript)*
    data_stmt_object_list   = data_stmt_object (_0 comma _0 data_stmt_object)*
    bounds_remapping_list   = bounds_remapping (_0 comma _0 bounds_remapping)*
    data_i_do_object_list   = data_i_do_object (_0 comma _0 data_i_do_object)*
    case_value_range_list   = case_value_range (_0 comma _0 case_value_range)*
    data_stmt_value_list    = data_stmt_value (_0 comma _0 data_stmt_value)*
    type_param_spec_list    = type_param_spec (_0 comma _0 type_param_spec)*
    actual_arg_spec_list    = actual_arg_spec (_0 comma _0 actual_arg_spec)*
    type_param_name_list    = type_param_name (_0 comma _0 type_param_name)*
    equivalence_set_list    = equivalence_set (_0 comma _0 equivalence_set)*
    type_param_decl_list    = type_param_decl (_0 comma _0 type_param_decl)*
    io_control_spec_list    = io_control_spec (_0 comma _0 io_control_spec)*
    allocate_object_list    = allocate_object (_0 comma _0 allocate_object)*
    pointer_object_list     = pointer_object (_0 comma _0 pointer_object)*
    type_attr_spec_list     = type_attr_spec (_0 comma _0 type_attr_spec)*
    component_decl_list     = component_decl (_0 comma _0 component_decl)*
    procedure_name_list     = procedure_name (_0 comma _0 procedure_name)*
    component_spec_list     = component_spec (_0 comma _0 component_spec)*
    dummy_arg_name_list     = dummy_arg_name (_0 comma _0 dummy_arg_name)*
    external_name_list      = external_name (_0 comma _0 external_name)*
    position_spec_list      = position_spec (_0 comma _0 position_spec)*
    implicit_spec_list      = implicit_spec (_0 comma _0 implicit_spec)*
    pointer_decl_list       = pointer_decl (_0 comma _0 pointer_decl)*
    connect_spec_list       = connect_spec (_0 comma _0 connect_spec)*
    binding_attr_list       = binding_attr (_0 comma _0 binding_attr)*
    binding_name_list       = binding_name (_0 comma _0 binding_name)*
    saved_entity_list       = saved_entity (_0 comma _0 saved_entity)*
    inquire_spec_list       = inquire_spec (_0 comma _0 inquire_spec)*
    output_item_list        = output_item (_0 comma _0 output_item)*
    dealloc_opt_list        = dealloc_opt (_0 comma _0 dealloc_opt)*
    entity_decl_list        = entity_decl (_0 comma _0 entity_decl)*
    import_name_list        = import_name (_0 comma _0 import_name)*
    letter_spec_list        = letter_spec (_0 comma _0 letter_spec)*
    bind_entity_list        = bind_entity (_0 comma _0 bind_entity)*
    bounds_spec_list        = bounds_spec (_0 comma _0 bounds_spec)*
    association_list        = association (_0 comma _0 association)*
    object_name_list        = object_name (_0 comma _0 object_name)*
    format_item_list        = format_item (_0 comma _0 format_item)*
    entity_name_list        = entity_name (_0 comma _0 entity_name)*
    enumerator_list         = enumerator (_0 comma _0 enumerator)*
    input_item_list         = input_item (_0 comma _0 input_item)*
    close_spec_list         = close_spec (_0 comma _0 close_spec)*
    flush_spec_list         = flush_spec (_0 comma _0 flush_spec)*
    alloc_opt_list          = alloc_opt (_0 comma _0 alloc_opt)*
    proc_decl_list          = proc_decl (_0 comma _0 proc_decl)*
    wait_spec_list          = wait_spec (_0 comma _0 wait_spec)*
    dummy_arg_list          = dummy_arg (_0 comma _0 dummy_arg)*
    access_id_list          = access_id (_0 comma _0 access_id)*
    ac_value_list           = ac_value (_0 comma _0 ac_value)*
    rename_list             = rename (_0 comma _0 rename)*
    label_list              = label (_0 comma _0 label)*
    only_list               = only (_0 comma _0 only)*
    v_list                  = v (_0 comma _0 v)*

    allocation_list         = allocation (_0 comma _0 !alloc_opt allocation)*

    ################## variables ###################
    iomsg_variable          = scalar_default_char_variable
    errmsg_variable         = scalar_default_char_variable
    stat_variable           = scalar_int_variable
    data_i_do_variable      = scalar_int_variable
    do_variable             = scalar_int_variable
    ac_do_variable          = scalar_int_variable
    scalar_int_variable     = int_variable
    scalar_default_char_variable = default_char_variable
    scalar_default_logical_variable = default_logical_variable
    internal_file_variable  = char_variable
    default_logical_variable= variable
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
    proc_entity_name        = name
    entry_name              = name

    ################## base terms ###################
    letter_spec             = letter letter_range
    letter_range            = (_0 minus _0 letter )?
    name                    = letter ident_re
    label                   = fivedigit
    exponent_letter         = ~"[ED]"i
    sign                    = plus / minus
    digit_string            = ~"[0-9]+"
    letter                  = ~"[A-Z]"i
    rep_char                = ~"{smapstr}"

    ################## regular expressions and basic literal terms ###################
    dot                     = "."
    plus                    = "+"
    star                    = "*"
    minus                   = "-"
    slash                   = "/"
    equal                   = "="
    comma                   = ","
    colon                   = ":"
    lslash                  = "(/"
    rslash                  = "/)"
    dequal                  = "=="
    squote                  = "'"
    dquote                  = "\""
    lparen                  = "("
    rparen                  = ")"
    langle                  = "<"
    rangle                  = ">"
    points                  = "=>"
    percent                 = "%"
    dcolons                 = "::"
    lbracket                = "["
    rbracket                = "]"
    notequal                = "/="
    lessequal               = "<="
    underscore              = "_"
    greaterequal            = ">="
    a_re                    = ~"A"i
    b_re                    = ~"B"i
    c_re                    = ~"C"i
    d_re                    = ~"D"i
    e_re                    = ~"E"i
    f_re                    = ~"F"i
    g_re                    = ~"G"i
    i_re                    = ~"I"i
    l_re                    = ~"L"i
    o_re                    = ~"O"i
    p_re                    = ~"P"i
    s_re                    = ~"S"i
    t_re                    = ~"T"i
    z_re                    = ~"Z"i
    x_re                    = ~"X"i
    bn_re                   = ~"BN"i
    bz_re                   = ~"BZ"i
    dc_re                   = ~"DC"i
    do_re                   = ~"DO"i
    dp_re                   = ~"DP"i
    dt_re                   = ~"DT"i
    en_re                   = ~"EN"i
    es_re                   = ~"ES"i
    go_re                   = ~"GO"i
    id_re                   = ~"ID"i
    if_re                   = ~"IF"i
    in_re                   = ~"IN"i
    is_re                   = ~"IS"i
    rc_re                   = ~"RC"i
    rd_re                   = ~"RD"i
    rn_re                   = ~"RN"i
    rp_re                   = ~"RP"i
    ru_re                   = ~"RU"i
    rz_re                   = ~"RZ"i
    sp_re                   = ~"SP"i
    ss_re                   = ~"SS"i
    tl_re                   = ~"TL"i
    to_re                   = ~"TO"i
    tr_re                   = ~"TR"i
    end_re                  = ~"END"i
    eor_re                  = ~"EOR"i
    err_re                  = ~"ERR"i
    fmt_re                  = ~"FMT"i
    len_re                  = ~"LEN"i
    nml_re                  = ~"NML"i
    out_re                  = ~"OUT"i
    pad_re                  = ~"PAD"i
    pos_re                  = ~"POS"i
    rec_re                  = ~"REC"i
    use_re                  = ~"USE"i
    bind_re                 = ~"BIND"i
    call_re                 = ~"CALL"i
    case_re                 = ~"CASE"i
    data_re                 = ~"DATA"i
    else_re                 = ~"ELSE"i
    enum_re                 = ~"ENUM"i
    exit_re                 = ~"EXIT"i
    file_re                 = ~"FILE"i
    form_re                 = ~"FORM"i
    kind_re                 = ~"KIND"i
    name_re                 = ~"NAME"i
    none_re                 = ~"NONE"i
    only_re                 = ~"ONLY"i
    open_re                 = ~"OPEN"i
    pass_re                 = ~"PASS"i
    pure_re                 = ~"PURE"i
    read_re                 = ~"READ"i
    real_re                 = ~"REAL"i
    recl_re                 = ~"RECL"i
    save_re                 = ~"SAVE"i
    sign_re                 = ~"SIGN"i
    size_re                 = ~"SIZE"i
    stat_re                 = ~"STAT"i
    stop_re                 = ~"STOP"i
    then_re                 = ~"THEN"i
    type_re                 = ~"TYPE"i
    unit_re                 = ~"UNIT"i
    wait_re                 = ~"WAIT"i
    blank_re                = ~"BLANK"i
    block_re                = ~"BLOCK"i
    class_re                = ~"CLASS"i
    close_re                = ~"CLOSE"i
    cycle_re                = ~"CYCLE"i
    delim_re                = ~"DELIM"i
    entry_re                = ~"ENTRY"i
    exist_re                = ~"EXIST"i
    final_re                = ~"FINAL"i
    flush_re                = ~"FLUSH"i
    iomsg_re                = ~"IOMSG"i
    named_re                = ~"NAMED"i
    print_re                = ~"PRINT"i
    round_re                = ~"ROUND"i
    value_re                = ~"VALUE"i
    where_re                = ~"WHERE"i
    while_re                = ~"WHILE"i
    write_re                = ~"WRITE"i
    access_re               = ~"ACCESS"i
    action_re               = ~"ACTION"i
    common_re               = ~"COMMON"i
    direct_re               = ~"DIRECT"i
    double_re               = ~"DOUBLE"i
    errmsg_re               = ~"ERRMSG"i
    forall_re               = ~"FORALL"i
    format_re               = ~"FORMAT"i
    import_re               = ~"IMPORT"i
    intent_re               = ~"INTENT"i
    iostat_re               = ~"IOSTAT"i
    module_re               = ~"MODULE"i
    nopass_re               = ~"NOPASS"i
    number_re               = ~"NUMBER"i
    opened_re               = ~"OPENED"i
    public_re               = ~"PUBLIC"i
    result_re               = ~"RESULT"i
    return_re               = ~"RETURN"i
    rewind_re               = ~"REWIND"i
    select_re               = ~"SELECT"i
    source_re               = ~"SOURCE"i
    status_re               = ~"STATUS"i
    stream_re               = ~"STREAM"i
    target_re               = ~"TARGET"i
    advance_re              = ~"ADVANCE"i
    complex_re              = ~"COMPLEX"i
    convert_re              = ~"CONVERT"i
    decimal_re              = ~"DECIMAL"i
    default_re              = ~"DEFAULT"i
    endfile_re              = ~"ENDFILE"i
    extends_re              = ~"EXTENDS"i
    generic_re              = ~"GENERIC"i
    inquire_re              = ~"INQUIRE"i
    integer_re              = ~"INTEGER"i
    logical_re              = ~"LOGICAL"i
    nextrec_re              = ~"NEXTREC"i
    nullify_re              = ~"NULLIFY"i
    pending_re              = ~"PENDING"i
    pointer_re              = ~"POINTER"i
    private_re              = ~"PRIVATE"i
    program_re              = ~"PROGRAM"i
    abstract_re             = ~"ABSTRACT"i
    allocate_re             = ~"ALLOCATE"i
    contains_re             = ~"CONTAINS"i
    continue_re             = ~"CONTINUE"i
    deferred_re             = ~"DEFERRED"i
    encoding_re             = ~"ENCODING"i
    external_re             = ~"EXTERNAL"i
    function_re             = ~"FUNCTION"i
    implicit_re             = ~"IMPLICIT"i
    iolength_re             = ~"IOLENGTH"i
    namelist_re             = ~"NAMELIST"i
    operator_re             = ~"OPERATOR"i
    optional_re             = ~"OPTIONAL"i
    position_re             = ~"POSITION"i
    sequence_re             = ~"SEQUENCE"i
    volatile_re             = ~"VOLATILE"i
    associate_re            = ~"ASSOCIATE"i
    backspace_re            = ~"BACKSPACE"i
    dimension_re            = ~"DIMENSION"i
    character_re            = ~"CHARACTER"i
    elemental_re            = ~"ELEMENTAL"i
    elsewhere_re            = ~"ELSEWHERE"i
    formatted_re            = ~"FORMATTED"i
    interface_re            = ~"INTERFACE"i
    intrinsic_re            = ~"INTRINSIC"i
    parameter_re            = ~"PARAMETER"i
    precision_re            = ~"PRECISION"i
    procedure_re            = ~"PROCEDURE"i
    protected_re            = ~"PROTECTED"i
    readwrite_re            = ~"READWRITE"i
    recursive_re            = ~"RECURSIVE"i
    assignment_re           = ~"ASSIGNMENT"i
    deallocate_re           = ~"DEALLOCATE"i
    enumerator_re           = ~"ENUMERATOR"i
    sequential_re           = ~"SEQUENTIAL"i
    subroutine_re           = ~"SUBROUTINE"i
    allocatable_re          = ~"ALLOCATABLE"i
    equivalence_re          = ~"EQUIVALENCE"i
    unformatted_re          = ~"UNFORMATTED"i
    asynchronous_re         = ~"ASYNCHRONOUS"i
    non_intrinsic_re        = ~"NON_INTRINSIC"i
    non_overridable_re      = ~"NON_OVERRIDABLE"i
    bdigit                  = ~"[0-1]+"
    odigit                  = ~"[0-7]+"
    hdigit                  = ~"[A-F0-9]+"i
    fivedigit               = ~"[0-9]{{1,5}}"
    ident_re                = ~"[_A-Z0-9]{{0,63}}"i
    eq_re                   = ~"\.EQ\."i
    ne_re                   = ~"\.NE\."i
    lt_re                   = ~"\.LT\."i
    le_re                   = ~"\.LE\."i
    gt_re                   = ~"\.GT\."i
    ge_re                   = ~"\.GE\."i
    eqv_re                  = ~"\.EQV\."i
    neqv_re                 = ~"\.NEQV\."i
    true_re                 = ~"\.TRUE\."i
    false_re                = ~"\.FALSE\."i

    ################## utilities ###################
    _CL                     = _C / _B
    _C                      = _0 comment EOL
    _B                      = _0 EOL
    _L                      = _0 (label _1)?
    comment                 = ~"{cmapstr}"
    EOL                     = ~"[\r\n]"
    _1                      = ~"[ \t]+"
    _0                      = ~"[ \t]*"
    _S                      = ~"[ \t]"
""".format(
    smapstr=utils.SMAPSTR.replace('%d', '[\d]+'),
    cmapstr=utils.CMAPSTR.replace('%d', '[\d]+'),
    fmapstr=utils.FMAPSTR.replace('%d', '[\d]+')
)

f2003_grammar = parsergen.Grammar(f2003_grammar_spec)

def is_defined(rule):
    return True if re.search(r"\n[ \t]*%s\s*="%rule,
        f2003_grammar_spec) else False

def defined_rules():
    find = re.findall(r"\n[ \t]*[_A-Z0-9]+\s*=",
        f2003_grammar_spec, re.I)
    return [i.split()[0] for i in find] if find else []

def parse(preprocessed, pos=0, lift_child=True,
        remove_blanknode=True, apply_stringmap=True,
        apply_commentmap=True):

    def rulename(parent, node, depth, bag):
        if node.node.expr.name not in bag:
            bag.append(node.node.expr.name)

    #import pdb; pdb.set_trace()
    tree = f2003_grammar.parse(preprocessed,
        pos=pos,
        lift_child=lift_child,
        remove_blanknode=remove_blanknode,
        apply_stringmap=apply_stringmap,
        apply_commentmap=apply_commentmap)
    #ret = tree.topdown_visit(rulename=rulename)
    #ret = tree.bottomup_visit(rulename=rulename)
    #tree.showtree()
    #print(str(tree))
    #print(str(ret))
    return tree

