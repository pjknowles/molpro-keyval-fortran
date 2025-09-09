module molpro_input
    private
    public :: convert_keyval_to_json, generate_molpro_input
    type :: keyval
        character(:), allocatable :: key
        character(:), allocatable :: value
    end type keyval
contains
    logical function istrue()
        istrue = .true.

    end function istrue

    logical function isfalse()
        isfalse = .false.

    end function isfalse


    function split_quote_protected_string(string, delimiter) result(result)
        character(1024), allocatable, dimension(:) :: result
        character(*), intent(in) :: string
        character(1), intent(in) :: delimiter
        character(1024), allocatable, dimension(:) :: result_
        character(:), allocatable :: string_
        character(1) :: s
        logical :: quoted
        integer :: i, l
        allocate(result(0))
        if (string.eq.'') return
        !        print *, 'split_quote_protected_string ', string
        string_ = delimiter // string
        quoted = .false.
        do i = 1, len_trim(string_)
            s = string_(i:i)
            if (.not. quoted .and. s == delimiter) then
                l = ubound(result, 1) + 1
                !                print *, 'extending to l=', l
                allocate(result_(l - 1))
                result_ = result
                deallocate(result)
                allocate(result(l))
                result(:l - 1) = result_
                result(l) = ''
                deallocate(result_)
            else
                if (quoted .and. (s.eq.'''' .or. s.eq.'"')) then
                    quoted = .false.
                elseif (.not. quoted .and. (s.eq.'''' .or. s.eq.'"')) then
                    quoted = .true.
                end if
                !                print *, 'adopt ', s
                result(ubound(result, 1)) = trim(result(ubound(result, 1))) // s
                !                print *, result
            end if
        end do
    end function split_quote_protected_string

    function convert_keyval_to_json(string) result(result)
        character(:), allocatable :: result
        character(*), intent(in) :: string
        character(:), allocatable, dimension(:) :: split_string, split_basis_string
        character(:), allocatable :: key, value
        integer :: i, j, equal_position
        !        print *, 'convert_keyval_to_json', string
        result = ''
        split_string = split_quote_protected_string(string, ',')
        do i = lbound(split_string, 1), ubound(split_string, 1)
            !                        print *, 'split_string(', i, ') ', split_string(i)
            equal_position = index(split_string(i), '=')
            key = trim(adjustl(split_string(i)(:equal_position - 1)))
            value = trim(adjustl(split_string(i)(equal_position + 1:)))
            if (index('''"', key(1:1)) .gt. 0) key = adjustl(key(2:))
            if (index('''"', key(len(key):len(key))) .gt. 0) key = trim(key(:len(key) - 1))
            if (index('''"', value(1:1)) .gt. 0) value = adjustl(value(2:))
            if (index('''"', value(len(value):len(value))) .gt. 0) value = trim(value(:len(value) - 1))
            if (key.eq.'basis' .and. index(value, 'default') .eq.0) value = 'default=' // value
            if (key.eq.'basis') then
                split_basis_string = split_quote_protected_string(value, ',')
                value = '{'
                do j = lbound(split_basis_string, 1), ubound(split_basis_string, 1)
                    if (j.eq.2) value = value // '"elements":{'
                    equal_position = index(split_basis_string(j), '=')
                    value = value // '"' // adjustl(split_basis_string(j)(:equal_position - 1)) // &
                            '":"' // trim(adjustl(split_basis_string(j)(equal_position + 1:))) // '",'
                    if (j.eq.ubound(split_basis_string,1)) then
                     if (j.eq.1) then
                         value(len_trim(value):) = '}'
                         else
                    value = value(:len_trim(value)-1) //'}}'
                     end if
                    end if
                end do
!                print *, 'final basis string ', value
            else !if (value.ne.'True' .and. value.ne.'False') then
                value = '"' // value // '"'
            end if
            !            print *, 'key', key, 'value', value
            if (result.eq.'') then
                result = '{"' // key // '": ' // value
            else
                result = result // ', "' // key // '": ' // value
            end if

        end do
        if (result.eq.'') result = '{'
        result = result // '}'
        !        print *, 'convert_keyval_to_json returns', result
    end function convert_keyval_to_json


    !> Create a Molpro command-style input from a JSON declarative input, or its keyval equivalent
    function generate_molpro_input(string) result(result)
        use :: json_module, rk => json_rk
        implicit none
        type(json_file) :: json
        type(json_file) :: schema
        logical :: is_found
        character(:), allocatable :: result
        character(*), intent(in) :: string
        character(:), allocatable :: json_strng
        logical(kind = json_lk) :: status_ok
        character(:), allocatable :: cval, basis_spec, key, string2, job_type, string3
        type(json_value), pointer :: value, value2
        type(json_core) :: core
        integer :: i, j
        type(keyval), dimension(:), allocatable :: keyvals
        character(len=2048) :: path

        call schema%initialize(path_separator = '/')
        call get_command_argument(0, path)
        path = path(:index(path,'/',.true.))//'molpro_input.json'
        call schema%load(path)
!                call schema%print
        !        print *, 'default geometry: ', schema_get('properties/geometry/default')
        !        print *, 'default basis: ', schema_get('properties/basis/properties/default/default')

        result = ''
        call json%initialize(path_separator = '/')
        if (string(1:1).eq.'{') then
            json_strng = string
        else
            json_strng = convert_keyval_to_json(string)
        end if
        !        print *, 'json_strng: ', json_strng
        call json%load_from_string(json_strng)
        call json%get_core(core)
        call json%check_for_errors(status_ok, cval)
        !        print *, status_ok
        if (.not.status_ok) print *, cval
        !        call json%print

        !        cval = get('basis/default', default = .true.)
        !        print *, 'defaulted basis/default: ', cval
        !        cval = get('basis/default', default = .false.)
        !        print *, 'undefaulted basis/default: ', cval
        !        cval = get('hamiltonian', default = .true.)
        !        print *, 'defaulted hamiltonian: ', cval
        !        cval = get('hamiltonian', default = .false.)
        !        print *, 'undefaulted hamiltonian: ', cval

        call add_input('prologue')

        call json%get('orientation', cval, is_found)
        if (is_found) then
            if (cval.eq.'none') then
                call put('orient,noorient')
            else
                call put('orient' // cval)
            end if
        end if

        call json%get('symmetry', cval, is_found)
        if (is_found .and. cval.eq.'none') call put('symmetry,nosym')

        call json%get('angstrom', cval, is_found)
        if (is_found .and. cval.ne.'') call put('angstrom')

        call json%get('geometry', cval, is_found)
        if (is_found) then
            !            print *, 'geometry_found', cval
            inquire (file = cval, exist = status_ok)
            if (status_ok .or. cval(len_trim(cval) - 2:).eq.'.h5' .or. cval(len_trim(cval) - 3:).eq.'.xyz' .or. cval(1:1).eq.'{') then
                call put('geometry=' // cval)
            else
                call put('geometry={' // cval // '}')
            endif
        end if

        call json%get('basis', value, is_found)
        if (is_found) then
            basis_spec = 'basis=' // get('basis/default')
            call json%get('basis/elements', value, is_found)
            if (is_found) then
                keyvals = get_keyvals('basis/elements')
                do i = lbound(keyvals, 1), ubound(keyvals, 1)
                    basis_spec = basis_spec // ',' // keyvals(i)%key // '=' // keyvals(i)%value
                end do
            end if
            call put(basis_spec)
        end if

        call json%get('hamiltonian', cval, is_found)
        if (is_found .and. cval(1:2).eq.'DK') then
            i = 1
            if (len(cval).eq.3) read(cval(3:), '(I1)') i
            call put('dkho=' // int_to_str(i))
        end if

        call json%get('variables', value, is_found)
        if (is_found) then
            keyvals = get_keyvals('variables')
            do i = lbound(keyvals, 1), ubound(keyvals, 1)
                if (keyvals(i)%value.ne.'' .and. (keyvals(i)%key .ne. 'charge' .or. keyvals(i)%value .ne. '0')) call put(keyvals(i)%key // '=' // keyvals(i)%value)
            end do
        end if

        call json%get('core_correlation', cval, is_found)
        if (is_found) call put('core,' // cval)

        ! TODO properly implement multi-step methods
        call json%get('geometry', string2, is_found)
        if (is_found) then

            call put('proc ansatz')
            string2 = get('method', default = .true.)
            !        print*,'method', string2
            string3 = string2//','
            string3 = string3(:index(string3,',')-1)
            if (index('ur', string3(1:1)).gt.0) string3 = string3(2:)
            if (string3(1:2).ne.'hf' .and. string3(1:2).ne.'ks') then
                string3 = 'hf'
                if (get('density_fitting').ne.'') string3 = 'df-' // string3
                call put('{'//string3//'}')
            end if

            if (get('density_fitting').ne.'' .and. string2(:4) .ne. 'pno-' .and.string2(:4) .ne. 'ldf-') string2 = 'df-' // string2
            call put('{'//string2//'}')
            call put('endproc')

            job_type = get('job_type', .true.)
            !TODO drive off job json instead of always schema defaults
            do i = 1, 100
                string2 = schema_get('properties/job_type_commands/items/' // job_type // '/default(' // int_to_str(i) // ')')
                if (string2.eq.'' .and. i.eq.1) call put('{ansatz}')
                if (string2.eq.'') exit
                string2 = string2 // ';'
                j = index(string2, ';')
                string2 = string2(:j - 1) // ',proc=ansatz' // string2(j:)
                string2 = '{' // string2(:len_trim(string2) - 1) // '}'
                call put(string2)
            end do
        end if


    contains

        function get_keyvals(path) result(keyvals)
            character(*), intent(in) :: path
            type(json_value), pointer :: node, element
            type(keyval), dimension(:), allocatable :: keyvals
            type(json_value), pointer :: value
            character(:), allocatable :: string, element_path
            integer :: n
            call json%get(path, node, is_found)
            n = core%count(node)
            !            print *, 'in get_keyvals, n=', n
            !            call core%print(node)
            allocate(keyvals(n))
            do i = 1, n ! there must be an easier way to get the keys!
                element_path = path // '(' // int_to_str(i) // ')'
                !                print *, 'element_path', element_path
                call json%get(element_path, element, is_found)
                !                print *, 'is_found ', is_found
                !                call core%print(element)
                call core%get_path(element, string)
                !                print *, 'string ', string, index(string, '/', .true.)
                key = string(index(string, '/', .true.) + 1:)
                !                print *, 'key', key
                call core%print_to_string(element, string)
                !                print *, 'value', string
                string = string(2:len_trim(string) - 2)
                !                print *, 'value', string
                keyvals(i) = keyval(key, string)
            end do
        end function get_keyvals

        recursive pure function int_to_str(i) result(result)
            character(:), allocatable :: result
            integer, intent(in) :: i
            integer :: j
            result = ''
            if (i.lt.10) then
                result = char(ichar('0') + i)
            elseif (i.lt.100) then
                j = i / 10
                result = int_to_str(j) // int_to_str(i - j * 10)
            end if
        end function int_to_str

        subroutine add_input(keyword, default)
            character(*), intent(in) :: keyword
            logical, optional :: default
            call put(get(keyword, default))
        end subroutine add_input

        subroutine put(string)
            character(*), intent(in) :: string
            if (string.ne.'') result = result // string // new_line(' ')
        end subroutine put

        function get(path, default) result(result)
            character(:), allocatable :: result
            logical, optional :: default
            character(*), intent(in) :: path
            character(:), allocatable :: schema_path, res
            logical(kind = json_lk) :: status_ok
            type(json_value), pointer :: value, value2
            integer :: i
            !                                    print *, 'get ', path, present(default)
            call json%get(path, result, status_ok)
            !                        print *,status_ok
            if (.not. status_ok) then
                result = ''
                ! is it instead a list?
                !                print *, 'path ', path
                call json%get(path, value, status_ok)
                !                                print *, 'got json_value? ', status_ok
                if (status_ok) then
                    !                                    print *, 'got something'
                    do i = 1, 99
                        call json%get(trim(path) // '(' // int_to_str(i) // ')', res, status_ok)
!                        if (status_ok) print *, 'loop got ', res
                        result = result // res // new_line(' ')
!                        print *, 'result now ', result
                        if (.not. status_ok) exit
                    end do
                end if
                !            print *, 'path: ', path
                if (result .eq. '' .and. present(default)) then
                    !                    if (default) print *, 'trying schema_default ', path
                    if (default) result = schema_default(path)
                    !                    if (default) print *, 'result', result
                end if
            end if
            do while (result(len(result):) .eq. new_line(' '))
                result = result(:len(result) - 1)
            end do
            !            print *,'result @',result,'@'
        end function get

        function schema_default(path) result(result)
            character(*), intent(in) :: path
            character(:), allocatable :: result
            character(:), allocatable :: schema_path
            logical(kind = json_lk) :: status_ok
            integer :: pos, newpos
            schema_path = ''
            pos = 1
            do while (len_trim(path(pos:)).gt.1 .and. index(trim(path(pos:)) // '/', '/') .gt. 0)
                newpos = index(trim(path(pos:)) // '/', '/') + pos
                schema_path = schema_path // 'properties/' // path(pos:newpos - 2) // '/'
                pos = newpos
            end do
            schema_path = schema_path // 'default'
            call schema%get(schema_path, result, status_ok)
            !            print *, 'path: ', path
            !            print *, 'schema_path: ', status_ok
            if (.not. status_ok) result = ''
        end function schema_default

        function schema_get(path) result(result)
            character(*), intent(in) :: path
            character(:), allocatable :: result
            logical(kind = json_lk) :: status_ok
            call schema%get(path, result, status_ok)
            !            print *, 'schema_get ',path, status_ok
            if (.not. status_ok) result = ''
        end function schema_get
    end function generate_molpro_input

end module molpro_input