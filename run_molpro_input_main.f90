program main
    use molpro_input, only : generate_molpro_input
    implicit none
    character(10240) :: string
    call get_command_argument(1, string)
    print *, 'keyword input: ', trim(string)
    string = generate_molpro_input(string)
    print *, 'generated input: ', trim(string)
end program main