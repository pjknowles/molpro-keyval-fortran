program main

    use fortuno_serial, only : execute_serial_cmd_app
    use testapp_tests, only : tests
    implicit none

    ! Register tests by providing name and subroutine to run for each test.
    ! Note: this routine does not return but stops the program with the right exit code.
    call execute_serial_cmd_app(tests())
end program main