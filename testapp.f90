module testapp_tests
    use molpro_input, only : generate_molpro_input, convert_keyval_to_json
    use fortuno_serial, only : is_equal, test => serial_case_item, check => serial_check, test_list
    implicit none

contains

    function canonicalise(string) result(result)
        character(*), intent(in) :: string
      character(:), allocatable :: result
        integer :: i
        result = string
        do i=1,len_trim(string)
           if (result(i:i) .eq. new_line(' ')) result(i:i) = ';'
        end do
        if (result(len(result):len(result)).eq.new_line(' ')) result = result(:len(result)-1)
    end function canonicalise

    subroutine check_strings(candidate, reference)
        character(*), intent(in) :: candidate, reference
        if (canonicalise(candidate) .ne. canonicalise(reference)) then
            print *, 'string match failure:'
            print *,'candidate: ',canonicalise(candidate)
            print *,'reference: ',canonicalise(reference)
        end if
        call check(canonicalise(candidate) .eq. canonicalise(reference))
    end subroutine check_strings

    !> Returns the tests in this module
    function tests()
        type(test_list) :: tests

        tests = test_list([&
                test("generate_molpro_input", test_generate_molpro_input) &
                        , test("convert_keyval_to_json", test_convert_keyval_to_json)&
                ])

    end function tests

    subroutine test_generate_molpro_input()
!        call check_strings(generate_molpro_input('geometry=He') , 'geometry={He};rhf')
        call check_strings(generate_molpro_input('prologue="file,2,orb.wf"') , 'file,2,orb.wf')
        call check_strings(generate_molpro_input('{"prologue":"file,2,orb.wf"}') , 'file,2,orb.wf')
        call check_strings(generate_molpro_input('{"prologue":["file,2,orb.wf"]}') , 'file,2,orb.wf')
    end subroutine test_generate_molpro_input

    subroutine test_convert_keyval_to_json
        call check(convert_keyval_to_json('') == '{}')
        call check(convert_keyval_to_json('a=123') == '{"a": "123"}')
        call check(convert_keyval_to_json('a=123,b=True,c=456') == '{"a": "123", "b": True, "c": "456"}')
        call check(convert_keyval_to_json('a=123,b="lots=of=equal=signs",c=456') == '{"a": "123", "b": "lots=of=equal=signs", "c": "456"}')
        call check(convert_keyval_to_json('basis=cc-pVTZ') == '{"basis": {"default":"cc-pVTZ"}}')
        call check(convert_keyval_to_json('basis="cc-pVTZ,Cu=cc-pVQZ-PP"') == '{"basis": {"default":"cc-pVTZ","elements":{"Cu":"cc-pVQZ-PP"}}')
        call check(convert_keyval_to_json('basis="cc-pVTZ,Zn=cc-pVTZ-PP,Cu=cc-pVQZ-PP"') == '{"basis": {"default":"cc-pVTZ","elements":{"Zn":"cc-pVTZ-PP","Cu":"cc-pVQZ-PP"}}')
        call check(convert_keyval_to_json('basis="cc-pVTZ,Zn=cc-pVTZ-PP,Cu=cc-pVQZ-PP"') == '{"basis": {"default":"cc-pVTZ","elements":{"Zn":"cc-pVTZ-PP","Cu":"cc-pVQZ-PP"}}')
    end subroutine test_convert_keyval_to_json

end module testapp_tests