program main
    use molpro_input, only : generate_molpro_input
    implicit none
    character(10240) :: string
    if (command_argument_count() .ge. 1) then
        call get_command_argument(1, string)
    else
        read '(A)', string
    end if
    print *, 'declarative input: ', trim(string)
    print *, 'generated input:', new_line(' '), generate_molpro_input(string)
end program main