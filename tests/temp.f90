!! R1001 format-stmt
!     is  FORMAT format-specification
!   
!! R1002 format-specification
!     is  ( [ format-item-list ] )
!     or  ( [ format-items, ] unlimited-format-item )
!
! Not [completely] tested here: format-item-list.
!
! Duplicate statements occur to test unparsing. These duplicates
! should be symantically the same (needs to be checked) and
! should unparse the same way.
!
FORMAT (I12, i1)
FORMAT(I12,I1)
format (1pe12.4)
FORMAT(1P,E12.4)
format (1PE12.4, I10)
FORMAT(1P,E12.4,I10)
!format (I12/'Dates:', 22I3)
!format (I12/I1, 22I3)
format (I12,/,'Dates:',22I3)
!format(//)
format(/,/)
format(:)
!format(:::)
format(:,:,:)
!format(::)
format(:,:)
format(I12,/,' Dates: ', 2(2I3,I5))
!format (I12 / 'Dates:', (2I3, I5))
FORMAT(I12,/,'Dates:', (2I3, I5))
!format (I12 / 'Dates:', 2(2I3, I5))
FORMAT(I12,/,'Dates:', 2(2I3,I5))
format (1x, i1, 1x, 'isn''t', 1x, i1)
FORMAT(1X,I1,1X,'isn''t',1X,I1)
format(1x, 2(f10.3, i5))
FORMAT(1X,2(F10.3,I5))
format (1x, f10.3, i5, f10.3, i5, f10.3, i5)
FORMAT(1X,F10.3,I5,F10.3,I5,F10.3,I5)

end
