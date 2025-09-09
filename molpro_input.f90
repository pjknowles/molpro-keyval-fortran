module molpro_input
    private
    public :: convert_keyval_to_json, generate_molpro_input
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
                end do
                value(len_trim(value):) = '}'
            else if (value.ne.'True' .and. value.ne.'False') then
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
        real(kind = rk) :: t0, dt, tf, mu
        real(kind = rk), allocatable :: x0(:)
        type(json_file) :: json
        type(json_file) :: schema
        logical :: is_found
        character(:), allocatable :: result
        character(*), intent(in) :: string
        character(:), allocatable :: json_strng
        logical(kind = json_lk) :: status_ok
        character(:), allocatable :: cval

        call schema%initialize(path_separator = '/')
        call schema%load('/Users/peterk/trees/molpro-keyval-fortran/molpro_input.json')
        call schema%print
        print *, 'default geometry: ', schema_get('properties/geometry/default')
        print *, 'default basis: ', schema_get('properties/basis/properties/default/default')
        cval = get('basis/default', default = .true.)
        print *, 'defaulted basis/default: ', cval
        cval = get('hamiltonian', default = .true.)
        print *, 'defaulted hamiltonian: ', cval
        cval = get('hamiltonian', default = .false.)
        print *, 'undefaulted hamiltonian: ', cval

        result = ''
        call json%initialize
        if (string(1:1).eq.'{') then
            json_strng = string
        else
            json_strng = convert_keyval_to_json(string)
        end if
        print *, 'json_strng: ', json_strng
        call json%load_from_string(json_strng)
        call json%check_for_errors(status_ok, cval)
        print *, status_ok
        if (.not.status_ok) print *, cval
        call json%print

        call add_input('prologue')
        call json%get('geometry', cval, is_found)
        if (is_found) then
            print *, 'geometry_found', cval
            inquire (file = cval, exist = status_ok)
            if (status_ok .or. cval(len_trim(cval) - 2:).eq.'.h5' .or. cval(len_trim(cval) - 3:).eq.'.xyz' .or. cval(1:1).eq.'{') then
                call put('geometry=' // cval)
            else
                call put('geometry={' // cval // '}')
            endif
        end if

    contains

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
            character(:), allocatable :: schema_path
            logical(kind = json_lk) :: status_ok
            print *, 'get ', path, present(default)
            call json%get(path, result, status_ok)
            if (status_ok) return
            result = ''
            print *, 'path: ', path
            if (present(default)) then
                if (default) then
                    result = schema_default(path)
                end if
            end if
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
            print *, 'path: ', path
            print *, 'schema_path: ', status_ok
            if (.not. status_ok) result = ''
        end function schema_default
        function schema_get(keyword) result(result)
            character(*), intent(in) :: keyword
            character(:), allocatable :: result
            logical(kind = json_lk) :: status_ok
            call schema%get(keyword, result, status_ok)
            if (.not. status_ok) result = ''
        end function schema_get
    end function generate_molpro_input

end module molpro_input