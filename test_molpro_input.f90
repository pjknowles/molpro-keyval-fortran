module test_molpro_input
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
        do while (index(result,';;').gt.0)
           i = index(result,';;')
            result = result(:i)//result(i+2:)
        end do
        if (result(len(result):len(result)).eq.';') result = result(:len(result)-1)
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
        call check_strings(generate_molpro_input('geometry=He') , 'geometry={He};proc ansatz;{hf};endproc;{ansatz}')
        call check_strings(generate_molpro_input('prologue="file,2,orb.wf"') , 'file,2,orb.wf')
        call check_strings(generate_molpro_input('{"prologue":"file,2,orb.wf"}') , 'file,2,orb.wf')
        call check_strings(generate_molpro_input('{"prologue":["file,1,orb.int","file,2,orb.wf"]}') , 'file,1,orb.int;file,2,orb.wf')
        call check_strings(generate_molpro_input('{"basis": {"default":"cc-pV5Z"}}'),'basis=cc-pV5Z')
        call check_strings(generate_molpro_input('{"basis": {"default":"cc-pV5Z", "elements":{"Cu":"cc-pVTZ-PP", "Zn":"cc-pVQZ-PP"}}}'),'basis=cc-pV5Z,Cu=cc-pVTZ-PP,Zn=cc-pVQZ-PP')
        call check_strings(generate_molpro_input('{"basis": {"default":"cc-PVQZ","elements":{"F":"aug-cc-pVQZ"}}}'),'basis=cc-PVQZ,F=aug-cc-pVQZ')
        call check_strings(generate_molpro_input('core_correlation=mixed') , 'core,mixed')
        call check_strings(generate_molpro_input('{"variables": {"a":"a1","b":"b2"}}') , 'a=a1;b=b2')
        call check_strings(generate_molpro_input('{"hamiltonian":"DK3"}') , 'dkho=3')
        call check_strings(generate_molpro_input('{"hamiltonian":"PP"}') , '')
        call check_strings(generate_molpro_input('{"geometry":"He", "job_type":"OPT+FREQ"}') , 'geometry={He};proc ansatz;{hf};endproc;{optg,savexyz=optimised.xyz,proc=ansatz};{frequencies,proc=ansatz;thermo}')
        call check_strings(generate_molpro_input('{"geometry":"He", "method":"mp2"}'),'geometry={He};proc ansatz;{hf};{mp2};endproc;{ansatz}')
        call check_strings(generate_molpro_input('{"geometry": "F;H,F,1.732", "method": "mp2", "job_type": "OPT+FREQ", "basis": {"default":"cc-PVQZ","elements":{"F":"aug-cc-pVQZ"}}}'),'geometry={F;H,F,1.732};basis=cc-PVQZ,F=aug-cc-pVQZ;proc ansatz;{hf};{mp2};endproc;{optg,savexyz=optimised.xyz,proc=ansatz};{frequencies,proc=ansatz;thermo}')
        call check_strings(generate_molpro_input('geometry="F;H,F,1.732",method=mp2,job_type=OPT+FREQ,basis="cc-PVQZ,F=aug-cc-pVQZ"'),'geometry={F;H,F,1.732};basis=cc-PVQZ,F=aug-cc-pVQZ;proc ansatz;{hf};{mp2};endproc;{optg,savexyz=optimised.xyz,proc=ansatz};{frequencies,proc=ansatz;thermo}')
    end subroutine test_generate_molpro_input

    subroutine test_convert_keyval_to_json
        call check_strings(convert_keyval_to_json('') , '{}')
        call check_strings(convert_keyval_to_json('a=123') , '{"a": "123"}')
        call check_strings(convert_keyval_to_json('a=123,b=True,c=456') , '{"a": "123", "b": "True", "c": "456"}')
        call check_strings(convert_keyval_to_json('a=123,b="lots=of=equal=signs",c=456') , '{"a": "123", "b": "lots=of=equal=signs", "c": "456"}')
        call check_strings(convert_keyval_to_json('basis=cc-pVTZ') , '{"basis": {"default":"cc-pVTZ"}}')
        call check(convert_keyval_to_json('basis="cc-pVTZ,Cu=cc-pVQZ-PP"') == '{"basis": {"default":"cc-pVTZ","elements":{"Cu":"cc-pVQZ-PP"}}}')
        call check(convert_keyval_to_json('basis="cc-pVTZ,Zn=cc-pVTZ-PP,Cu=cc-pVQZ-PP"') == '{"basis": {"default":"cc-pVTZ","elements":{"Zn":"cc-pVTZ-PP","Cu":"cc-pVQZ-PP"}}}')
        call check(convert_keyval_to_json('basis="cc-pVTZ,Zn=cc-pVTZ-PP,Cu=cc-pVQZ-PP"') == '{"basis": {"default":"cc-pVTZ","elements":{"Zn":"cc-pVTZ-PP","Cu":"cc-pVQZ-PP"}}}')
        call check_strings(convert_keyval_to_json('geometry="F;H,F,1.732",method=mp2,job_type=OPT+FREQ,basis="cc-PVQZ,F=aug-cc-pVQZ"'),'{"geometry": "F;H,F,1.732", "method": "mp2", "job_type": "OPT+FREQ", "basis": {"default":"cc-PVQZ","elements":{"F":"aug-cc-pVQZ"}}}')
    end subroutine test_convert_keyval_to_json

end module test_molpro_input