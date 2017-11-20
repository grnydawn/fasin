        program main
            errstring = ' '
            IF (.not. (do_cldice .or.        (associated(tnd_qsnow) .and. associated(tnd_nsnow) .and. associated(re_ice)))) THEN
                errstring = "MG's native cloud ice processes are disabled, but no replacement values were passed in."
            END IF
            IF (use_hetfrz_classnuc .and. (.not.        (associated(frzimm) .and. associated(frzcnt) .and. associated(frzdep)))) THEN
                errstring = "External heterogeneous freezing is enabled, but the required tendencies were not all passed in."
            END IF
        end program
