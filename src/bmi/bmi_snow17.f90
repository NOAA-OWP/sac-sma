module bmi_snow17_module

! NGEN_ACTIVE is to be set when running in the Nextgen framework
! https://github.com/NOAA-OWP/ngen
#ifdef NGEN_ACTIVE
   use bmif_2_0_iso
#else
   use bmif_2_0
#endif

  use runModule 
  use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_f_pointer
  implicit none

  type, extends (bmi) :: bmi_snow17
     private
     type (snow17_type) :: model
   contains
     procedure :: get_component_name => snow17_component_name
     procedure :: get_input_item_count => snow17_input_item_count
     procedure :: get_output_item_count => snow17_output_item_count
     procedure :: get_input_var_names => snow17_input_var_names
     procedure :: get_output_var_names => snow17_output_var_names
     procedure :: initialize => snow17_initialize
     procedure :: finalize => snow17_finalize
     procedure :: get_start_time => snow17_start_time
     procedure :: get_end_time => snow17_end_time
     procedure :: get_current_time => snow17_current_time
     procedure :: get_time_step => snow17_time_step
     procedure :: get_time_units => snow17_time_units
     procedure :: update => snow17_update
     procedure :: update_until => snow17_update_until
     procedure :: get_var_grid => snow17_var_grid
     procedure :: get_grid_type => snow17_grid_type
     procedure :: get_grid_rank => snow17_grid_rank
     procedure :: get_grid_shape => snow17_grid_shape
     procedure :: get_grid_size => snow17_grid_size
     procedure :: get_grid_spacing => snow17_grid_spacing
     procedure :: get_grid_origin => snow17_grid_origin
     procedure :: get_grid_x => snow17_grid_x
     procedure :: get_grid_y => snow17_grid_y
     procedure :: get_grid_z => snow17_grid_z
     procedure :: get_grid_node_count => snow17_grid_node_count
     procedure :: get_grid_edge_count => snow17_grid_edge_count
     procedure :: get_grid_face_count => snow17_grid_face_count
     procedure :: get_grid_edge_nodes => snow17_grid_edge_nodes
     procedure :: get_grid_face_edges => snow17_grid_face_edges
     procedure :: get_grid_face_nodes => snow17_grid_face_nodes
     procedure :: get_grid_nodes_per_face => snow17_grid_nodes_per_face
     procedure :: get_var_type => snow17_var_type
     procedure :: get_var_units => snow17_var_units
     procedure :: get_var_itemsize => snow17_var_itemsize
     procedure :: get_var_nbytes => snow17_var_nbytes
     procedure :: get_var_location => snow17_var_location
     procedure :: get_value_int => snow17_get_int
     procedure :: get_value_float => snow17_get_float
     procedure :: get_value_double => snow17_get_double
     generic :: get_value => &
          get_value_int, &
          get_value_float, &
          get_value_double
      procedure :: get_value_ptr_int => snow17_get_ptr_int
      procedure :: get_value_ptr_float => snow17_get_ptr_float
      procedure :: get_value_ptr_double => snow17_get_ptr_double
      generic :: get_value_ptr => &
           get_value_ptr_int, &
           get_value_ptr_float, &
           get_value_ptr_double
      procedure :: get_value_at_indices_int => snow17_get_at_indices_int
      procedure :: get_value_at_indices_float => snow17_get_at_indices_float
      procedure :: get_value_at_indices_double => snow17_get_at_indices_double
      generic :: get_value_at_indices => &
           get_value_at_indices_int, &
           get_value_at_indices_float, &
           get_value_at_indices_double
     procedure :: set_value_int => snow17_set_int
     procedure :: set_value_float => snow17_set_float
     procedure :: set_value_double => snow17_set_double
     generic :: set_value => &
          set_value_int, &
          set_value_float, &
          set_value_double
      procedure :: set_value_at_indices_int => snow17_set_at_indices_int
      procedure :: set_value_at_indices_float => snow17_set_at_indices_float
      procedure :: set_value_at_indices_double => snow17_set_at_indices_double
      generic :: set_value_at_indices => &
           set_value_at_indices_int, &
           set_value_at_indices_float, &
           set_value_at_indices_double
!      procedure :: print_model_info
  end type bmi_snow17

  private
  public :: bmi_snow17

  character (len=BMI_MAX_COMPONENT_NAME), target :: &
       component_name = "OWP Snow17 Module"

  ! Exchange items
  integer, parameter :: input_item_count = 2
  integer, parameter :: output_item_count = 4
  character (len=BMI_MAX_VAR_NAME), target, &
       dimension(input_item_count) :: input_items
  character (len=BMI_MAX_VAR_NAME), target, &
       dimension(output_item_count) :: output_items 

contains

  ! Get the name of the model.
  function snow17_component_name(this, name) result (bmi_status)
    class (bmi_snow17), intent(in) :: this
    character (len=*), pointer, intent(out) :: name
    integer :: bmi_status

    name => component_name
    bmi_status = BMI_SUCCESS
  end function snow17_component_name

  ! Count the input variables.
  function snow17_input_item_count(this, count) result (bmi_status)
    class (bmi_snow17), intent(in) :: this
    integer, intent(out) :: count
    integer :: bmi_status

    count = input_item_count
    bmi_status = BMI_SUCCESS
  end function snow17_input_item_count

  ! Count the output variables.
  function snow17_output_item_count(this, count) result (bmi_status)
    class (bmi_snow17), intent(in) :: this
    integer, intent(out) :: count
    integer :: bmi_status

    count = output_item_count
    bmi_status = BMI_SUCCESS
  end function snow17_output_item_count

  ! List input variables.
  function snow17_input_var_names(this, names) result (bmi_status)
    class (bmi_snow17), intent(in) :: this
    character (*), pointer, intent(out) :: names(:)
    integer :: bmi_status

    input_items(1) = 'tair'     ! mean air temperature (C)
    input_items(2) = 'precip'   ! total precipitation (mm)

    names => input_items
    bmi_status = BMI_SUCCESS
  end function snow17_input_var_names

  ! List output variables.
  function snow17_output_var_names(this, names) result (bmi_status)
    class (bmi_snow17), intent(in) :: this
    character (*), pointer, intent(out) :: names(:)
    integer :: bmi_status

    output_items(1) = 'precip_scf'   ! precip after scf scaling (mm)
    output_items(2) = 'sneqv'        ! snow water equivalent (mm)
    output_items(3) = 'snowh'        ! snow height (mm)
    output_items(4) = 'raim'         ! rain plus snowmelt (mm/s)

    names => output_items
    bmi_status = BMI_SUCCESS
  end function snow17_output_var_names

  ! BMI initializer.
  function snow17_initialize(this, config_file) result (bmi_status)
    class (bmi_snow17), intent(out) :: this
    character (len=*), intent(in) :: config_file
    integer :: bmi_status

    if (len(config_file) > 0) then
       call initialize_from_file(this%model, config_file)
    !else
       !call initialize_from_defaults(this%model)
     end if
    bmi_status = BMI_SUCCESS
  end function snow17_initialize

  ! BMI finalizer.
  function snow17_finalize(this) result (bmi_status)
    class (bmi_snow17), intent(inout) :: this
    integer :: bmi_status

    call cleanup(this%model)
    bmi_status = BMI_SUCCESS
  end function snow17_finalize

  ! Model start time.
  function snow17_start_time(this, time) result (bmi_status)
    class (bmi_snow17), intent(in) :: this
    double precision, intent(out) :: time
    integer :: bmi_status

    !time = 0.d0                                            ! time relative to start time (s) == 0
    time = dble(this%model%runinfo%start_datetime)         ! using unix time (s)
    
    bmi_status = BMI_SUCCESS
  end function snow17_start_time

  ! Model end time.
  function snow17_end_time(this, time) result (bmi_status)
    class (bmi_snow17), intent(in) :: this
    double precision, intent(out) :: time
    integer :: bmi_status

    !time = dble(this%model%runinfo%ntimes * this%model%runinfo%dt)  ! time relative to start time (s)
    time = dble(this%model%runinfo%end_datetime)                    ! using unix time (s)
    
    bmi_status = BMI_SUCCESS
  end function snow17_end_time

  ! Model current time.
  function snow17_current_time(this, time) result (bmi_status)
    class (bmi_snow17), intent(in) :: this
    double precision, intent(out) :: time
    integer :: bmi_status

    !time = dble(this%model%runinfo%time_dbl)           ! time from start of run (s)
    time = dble(this%model%runinfo%curr_datetime)    ! unix time (s)
    bmi_status = BMI_SUCCESS
  end function snow17_current_time

  ! Model time step.
  function snow17_time_step(this, time_step) result (bmi_status)
    class (bmi_snow17), intent(in) :: this
    double precision, intent(out) :: time_step
    integer :: bmi_status

    time_step = dble(this%model%runinfo%dt)
    bmi_status = BMI_SUCCESS
  end function snow17_time_step

  ! Model time units.
  function snow17_time_units(this, units) result (bmi_status)
    class (bmi_snow17), intent(in) :: this
    character (len=*), intent(out) :: units
    integer :: bmi_status

    units = "s"
    bmi_status = BMI_SUCCESS
  end function snow17_time_units

  ! Advance model by one time step.
  function snow17_update(this) result (bmi_status)
    class (bmi_snow17), intent(inout) :: this
    integer :: bmi_status

    call advance_in_time(this%model)
    bmi_status = BMI_SUCCESS
  end function snow17_update

  ! Advance the model until the given time.
  function snow17_update_until(this, time) result (bmi_status)
    class (bmi_snow17), intent(inout) :: this
    double precision, intent(in) :: time
    integer :: bmi_status
    double precision :: n_steps_real
    integer :: n_steps, i, s

    ! new code to work with unix time convention
    ! check to see if desired time to advance to is earlier than current time (can't go backwards)
    if (time < this%model%runinfo%curr_datetime) then
       bmi_status = BMI_FAILURE
       return
    end if
    ! otherwise try to advance to end time
    do while ( time < this%model%runinfo%end_datetime )
       s = this%update()
    end do

    ! original code working with time run convention from 0 to n*dt end_time
    !if (time < this%model%runinfo%time_dbl) then
    !   bmi_status = BMI_FAILURE
    !   return
    !end if

    !n_steps_real = (time - this%model%runinfo%time_dbl) / this%model%runinfo%dt
    !n_steps = floor(n_steps_real)
    !do i = 1, n_steps
    !   s = this%update()
    !end do
    !     call update_frac(this, n_steps_real - dble(n_steps)) ! NOT IMPLEMENTED
    
    bmi_status = BMI_SUCCESS
  end function snow17_update_until

  ! Get the grid id for a particular variable.
  function snow17_var_grid(this, name, grid) result (bmi_status)
    class (bmi_snow17), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, intent(out) :: grid
    integer :: bmi_status

    select case(name)
    case('tair', 'precip', &     ! input vars
         'precip_scf', 'sneqv', 'snowh', 'raim')        ! output vars
       grid = 0
       bmi_status = BMI_SUCCESS
    case default
       grid = -1
       bmi_status = BMI_FAILURE
    end select
  end function snow17_var_grid

  ! The type of a variable's grid.
  function snow17_grid_type(this, grid, type) result (bmi_status)
    class (bmi_snow17), intent(in) :: this
    integer, intent(in) :: grid
    character (len=*), intent(out) :: type
    integer :: bmi_status

    select case(grid)
    case(0)
       type = "scalar"
       bmi_status = BMI_SUCCESS
!================================ IMPLEMENT WHEN snow17 DONE IN GRID ======================
!     case(1)
!       type = "uniform_rectilinear"
!        bmi_status = BMI_SUCCESS
    case default
       type = "-"
       bmi_status = BMI_FAILURE
    end select
  end function snow17_grid_type

  ! The number of dimensions of a grid.
  function snow17_grid_rank(this, grid, rank) result (bmi_status)
    class (bmi_snow17), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: rank
    integer :: bmi_status

    select case(grid)
    case(0)
       rank = 0
       bmi_status = BMI_SUCCESS
!================================ IMPLEMENT WHEN snow17 DONE IN GRID ======================
!     case(1)
!        rank = 2
!        bmi_status = BMI_SUCCESS
    case default
       rank = -1
       bmi_status = BMI_FAILURE
    end select
  end function snow17_grid_rank

  ! The dimensions of a grid.
  function snow17_grid_shape(this, grid, shape) result (bmi_status)
    class (bmi_snow17), intent(in) :: this
    integer, intent(in) :: grid
    integer, dimension(:), intent(out) :: shape
    integer :: bmi_status

    select case(grid)
!================================ IMPLEMENT WHEN snow17 DONE IN GRID ======================
! NOTE: Scalar "grids" do not have dimensions, ie. there is no case(0)
!     case(1)
!        shape(:) = [this%model%n_y, this%model%n_x]
!        bmi_status = BMI_SUCCESS
    case default
       shape(:) = -1
       bmi_status = BMI_FAILURE
    end select
  end function snow17_grid_shape

  ! The total number of elements in a grid.
  function snow17_grid_size(this, grid, size) result (bmi_status)
    class (bmi_snow17), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: size
    integer :: bmi_status

    select case(grid)
    case(0)
       size = 1
       bmi_status = BMI_SUCCESS
!================================ IMPLEMENT WHEN snow17 DONE IN GRID ======================
!     case(1)
!        size = this%model%n_y * this%model%n_x
!        bmi_status = BMI_SUCCESS
    case default
       size = -1
       bmi_status = BMI_FAILURE
    end select
  end function snow17_grid_size

  ! The distance between nodes of a grid.
  function snow17_grid_spacing(this, grid, spacing) result (bmi_status)
    class (bmi_snow17), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: spacing
    integer :: bmi_status

    select case(grid)
!================================ IMPLEMENT WHEN snow17 DONE IN GRID ======================
! NOTE: Scalar "grids" do not have spacing, ie. there is no case(0)
!     case(1)
!        spacing(:) = [this%model%dy, this%model%dx]
!        bmi_status = BMI_SUCCESS
    case default
       spacing(:) = -1.d0
       bmi_status = BMI_FAILURE
    end select
  end function snow17_grid_spacing
!
  ! Coordinates of grid origin.
  function snow17_grid_origin(this, grid, origin) result (bmi_status)
    class (bmi_snow17), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: origin
    integer :: bmi_status

    select case(grid)
!================================ IMPLEMENT WHEN snow17 DONE IN GRID ======================
! NOTE: Scalar "grids" do not have coordinates, ie. there is no case(0)
!     case(1)
!        origin(:) = [0.d0, 0.d0]
!        bmi_status = BMI_SUCCESS
    case default
       origin(:) = -1.d0
       bmi_status = BMI_FAILURE
    end select
  end function snow17_grid_origin

  ! X-coordinates of grid nodes.
  function snow17_grid_x(this, grid, x) result (bmi_status)
    class (bmi_snow17), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: x
    integer :: bmi_status

    select case(grid)
    case(0)
       x(:) = [0.d0]
       bmi_status = BMI_SUCCESS
    case default
       x(:) = -1.d0
       bmi_status = BMI_FAILURE
    end select
  end function snow17_grid_x

  ! Y-coordinates of grid nodes.
  function snow17_grid_y(this, grid, y) result (bmi_status)
    class (bmi_snow17), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: y
    integer :: bmi_status

    select case(grid)
    case(0)
       y(:) = [0.d0]
       bmi_status = BMI_SUCCESS
    case default
       y(:) = -1.d0
       bmi_status = BMI_FAILURE
    end select
  end function snow17_grid_y

  ! Z-coordinates of grid nodes.
  function snow17_grid_z(this, grid, z) result (bmi_status)
    class (bmi_snow17), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: z
    integer :: bmi_status

    select case(grid)
    case(0)
       z(:) = [0.d0]
       bmi_status = BMI_SUCCESS
    case default
       z(:) = -1.d0
       bmi_status = BMI_FAILURE
    end select
  end function snow17_grid_z

  ! Get the number of nodes in an unstructured grid.
  function snow17_grid_node_count(this, grid, count) result(bmi_status)
    class(bmi_snow17), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: count
    integer :: bmi_status

    select case(grid)
    case(0:1)
       bmi_status = this%get_grid_size(grid, count)
    case default
       count = -1
       bmi_status = BMI_FAILURE
    end select
  end function snow17_grid_node_count

  ! Get the number of edges in an unstructured grid.
  function snow17_grid_edge_count(this, grid, count) result(bmi_status)
    class(bmi_snow17), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: count
    integer :: bmi_status

    count = -1
    bmi_status = BMI_FAILURE
  end function snow17_grid_edge_count

  ! Get the number of faces in an unstructured grid.
  function snow17_grid_face_count(this, grid, count) result(bmi_status)
    class(bmi_snow17), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: count
    integer :: bmi_status

    count = -1
    bmi_status = BMI_FAILURE
  end function snow17_grid_face_count

  ! Get the edge-node connectivity.
  function snow17_grid_edge_nodes(this, grid, edge_nodes) result(bmi_status)
    class(bmi_snow17), intent(in) :: this
    integer, intent(in) :: grid
    integer, dimension(:), intent(out) :: edge_nodes
    integer :: bmi_status

    edge_nodes(:) = -1
    bmi_status = BMI_FAILURE
  end function snow17_grid_edge_nodes

  ! Get the face-edge connectivity.
  function snow17_grid_face_edges(this, grid, face_edges) result(bmi_status)
    class(bmi_snow17), intent(in) :: this
    integer, intent(in) :: grid
    integer, dimension(:), intent(out) :: face_edges
    integer :: bmi_status

    face_edges(:) = -1
    bmi_status = BMI_FAILURE
  end function snow17_grid_face_edges

  ! Get the face-node connectivity.
  function snow17_grid_face_nodes(this, grid, face_nodes) result(bmi_status)
    class(bmi_snow17), intent(in) :: this
    integer, intent(in) :: grid
    integer, dimension(:), intent(out) :: face_nodes
    integer :: bmi_status

    face_nodes(:) = -1
    bmi_status = BMI_FAILURE
  end function snow17_grid_face_nodes

  ! Get the number of nodes for each face.
  function snow17_grid_nodes_per_face(this, grid, nodes_per_face) result(bmi_status)
    class(bmi_snow17), intent(in) :: this
    integer, intent(in) :: grid
    integer, dimension(:), intent(out) :: nodes_per_face
    integer :: bmi_status

    nodes_per_face(:) = -1
    bmi_status = BMI_FAILURE
  end function snow17_grid_nodes_per_face

  ! The data type of the variable, as a string.
  function snow17_var_type(this, name, type) result (bmi_status)
    class (bmi_snow17), intent(in) :: this
    character (len=*), intent(in) :: name
    character (len=*), intent(out) :: type
    integer :: bmi_status

    select case(name)
    case('tair', 'precip', &     ! input vars
         'precip_scf', 'sneqv', 'snowh', 'raim')        ! output vars
       type = "real"
       bmi_status = BMI_SUCCESS
    case default
       type = "-"
       bmi_status = BMI_FAILURE
    end select
  end function snow17_var_type

  ! The units of the given variable.
  function snow17_var_units(this, name, units) result (bmi_status)
    class (bmi_snow17), intent(in) :: this
    character (len=*), intent(in) :: name
    character (len=*), intent(out) :: units
    integer :: bmi_status

    select case(name)
    case("precip")
       units = "mm"
       bmi_status = BMI_SUCCESS
    case("tair")
       units = "C"
       bmi_status = BMI_SUCCESS
    case("precip_scf")
       units = "mm"
       bmi_status = BMI_SUCCESS
    case("sneqv")
       units = "mm"
       bmi_status = BMI_SUCCESS
    case("snowh")
       units = "mm"
       bmi_status = BMI_SUCCESS
    case("raim")
       units = "mm"
       bmi_status = BMI_SUCCESS
    case default
       units = "-"
       bmi_status = BMI_FAILURE
    end select
  end function snow17_var_units

  ! Memory use per array element.
  function snow17_var_itemsize(this, name, size) result (bmi_status)
    class (bmi_snow17), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, intent(out) :: size
    integer :: bmi_status

    select case(name)
    case("precip")
       size = sizeof(this%model%forcing%precip_comb)    ! 'sizeof' in gcc & ifort
       bmi_status = BMI_SUCCESS
    case("tair")
       size = sizeof(this%model%forcing%tair_comb)      ! 'sizeof' in gcc & ifort
       bmi_status = BMI_SUCCESS
    case("precip_scf")
       size = sizeof(this%model%forcing%precip_scf_comb)     ! 'sizeof' in gcc & ifort
       bmi_status = BMI_SUCCESS
    case("sneqv")
       size = sizeof(this%model%modelvar%sneqv_comb)     ! 'sizeof' in gcc & ifort
       bmi_status = BMI_SUCCESS
    case("snowh")
       size = sizeof(this%model%modelvar%snowh_comb)     ! 'sizeof' in gcc & ifort
       bmi_status = BMI_SUCCESS
    case("raim")
       size = sizeof(this%model%modelvar%raim_comb)      ! 'sizeof' in gcc & ifort
       bmi_status = BMI_SUCCESS
    case default
       size = -1
       bmi_status = BMI_FAILURE
    end select
  end function snow17_var_itemsize

  ! The size of the given variable.
  function snow17_var_nbytes(this, name, nbytes) result (bmi_status)
    class (bmi_snow17), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, intent(out) :: nbytes
    integer :: bmi_status
    integer :: s1, s2, s3, grid, grid_size, item_size

    s1 = this%get_var_grid(name, grid)
    s2 = this%get_grid_size(grid, grid_size)
    s3 = this%get_var_itemsize(name, item_size)

    if ((s1 == BMI_SUCCESS).and.(s2 == BMI_SUCCESS).and.(s3 == BMI_SUCCESS)) then
       nbytes = item_size * grid_size
       bmi_status = BMI_SUCCESS
    else
       nbytes = -1
       bmi_status = BMI_FAILURE
    end if
  end function snow17_var_nbytes

  ! The location (node, face, edge) of the given variable.
  function snow17_var_location(this, name, location) result (bmi_status)
    class (bmi_snow17), intent(in) :: this
    character (len=*), intent(in) :: name
    character (len=*), intent(out) :: location
    integer :: bmi_status
!==================== UPDATE IMPLEMENTATION IF NECESSARY WHEN RUN ON GRID =================
    select case(name)
    case default
       location = "node"
       bmi_status = BMI_SUCCESS
    end select
  end function snow17_var_location

  ! Get a copy of a integer variable's values, flattened.
  function snow17_get_int(this, name, dest) result (bmi_status)
    class (bmi_snow17), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, intent(inout) :: dest(:)
    integer :: bmi_status

    select case(name)
!==================== UPDATE IMPLEMENTATION IF NECESSARY FOR INTEGER VARS =================
!     case("model__identification_number")
!        dest = [this%model%id]
!        bmi_status = BMI_SUCCESS
    case default
       dest(:) = -1
       bmi_status = BMI_FAILURE
    end select
  end function snow17_get_int

  ! Get a copy of a real variable's values, flattened.
  function snow17_get_float(this, name, dest) result (bmi_status)
    class (bmi_snow17), intent(in) :: this
    character (len=*), intent(in) :: name
    real, intent(inout) :: dest(:)
    integer :: bmi_status

    select case(name)
    case("precip")
       dest(1) = this%model%forcing%precip_comb
       bmi_status = BMI_SUCCESS
    case("tair")
       dest(1) = this%model%forcing%tair_comb
       bmi_status = BMI_SUCCESS
    case("precip_scf")
       dest(1) = this%model%forcing%precip_scf_comb
       bmi_status = BMI_SUCCESS
    case("sneqv")
       dest(1) = this%model%modelvar%sneqv_comb
       bmi_status = BMI_SUCCESS
    case("snowh")
       dest(1) = this%model%modelvar%snowh_comb
       bmi_status = BMI_SUCCESS
    case("raim")
       dest(1) = this%model%modelvar%raim_comb
       bmi_status = BMI_SUCCESS
    case default
       dest(:) = -1.0
       bmi_status = BMI_FAILURE
    end select
    ! NOTE, if vars are gridded, then use:
    ! dest = reshape(this%model%temperature, [this%model%n_x*this%model%n_y]) 
  end function snow17_get_float

  ! Get a copy of a double variable's values, flattened.
  function snow17_get_double(this, name, dest) result (bmi_status)
    class (bmi_snow17), intent(in) :: this
    character (len=*), intent(in) :: name
    double precision, intent(inout) :: dest(:)
    integer :: bmi_status

    !==================== UPDATE IMPLEMENTATION IF NECESSARY FOR DOUBLE VARS =================

    select case(name)
    case default
       dest(:) = -1.d0
       bmi_status = BMI_FAILURE
    end select
  end function snow17_get_double

! !=================== get_value_ptr functions not implemented yet =================

   ! Get a reference to an integer-valued variable, flattened.
   function snow17_get_ptr_int(this, name, dest_ptr) result (bmi_status)
     class (bmi_snow17), intent(in) :: this
     character (len=*), intent(in) :: name
     integer, pointer, intent(inout) :: dest_ptr(:)
     integer :: bmi_status
     type (c_ptr) :: src
     integer :: n_elements

 !==================== UPDATE IMPLEMENTATION IF NECESSARY FOR INTEGER VARS =================

     select case(name)
     case default
        bmi_status = BMI_FAILURE
     end select
   end function snow17_get_ptr_int

   ! Get a reference to a real-valued variable, flattened.
   function snow17_get_ptr_float(this, name, dest_ptr) result (bmi_status)
     class (bmi_snow17), intent(in) :: this
     character (len=*), intent(in) :: name
     real, pointer, intent(inout) :: dest_ptr(:)
    integer :: bmi_status, status
     type (c_ptr) :: src
     integer :: n_elements, gridid

     select case(name)
     case default
        bmi_status = BMI_FAILURE
     end select
   end function snow17_get_ptr_float

   ! Get a reference to an double-valued variable, flattened.
   function snow17_get_ptr_double(this, name, dest_ptr) result (bmi_status)
     class (bmi_snow17), intent(in) :: this
     character (len=*), intent(in) :: name
     double precision, pointer, intent(inout) :: dest_ptr(:)
     integer :: bmi_status
     type (c_ptr) :: src
     integer :: n_elements

 !==================== UPDATE IMPLEMENTATION IF NECESSARY FOR DOUBLE VARS =================\

     select case(name)
     case default
        bmi_status = BMI_FAILURE
     end select
   end function snow17_get_ptr_double

   ! Get values of an integer variable at the given locations.
   function snow17_get_at_indices_int(this, name, dest, inds) &
        result (bmi_status)
     class (bmi_snow17), intent(in) :: this
     character (len=*), intent(in) :: name
     integer, intent(inout) :: dest(:)
     integer, intent(in) :: inds(:)
     integer :: bmi_status
     type (c_ptr) src
     integer, pointer :: src_flattened(:)
     integer :: i, n_elements

     select case(name)
     case default
        bmi_status = BMI_FAILURE
     end select
   end function snow17_get_at_indices_int

   ! Get values of a real variable at the given locations.
   function snow17_get_at_indices_float(this, name, dest, inds) &
        result (bmi_status)
     class (bmi_snow17), intent(in) :: this
    character (len=*), intent(in) :: name
     real, intent(inout) :: dest(:)
     integer, intent(in) :: inds(:)
     integer :: bmi_status
     type (c_ptr) src
     real, pointer :: src_flattened(:)
     integer :: i, n_elements

     select case(name)
     case default
        bmi_status = BMI_FAILURE
     end select
   end function snow17_get_at_indices_float

   ! Get values of a double variable at the given locations.
   function snow17_get_at_indices_double(this, name, dest, inds) &
        result (bmi_status)
     class (bmi_snow17), intent(in) :: this
     character (len=*), intent(in) :: name
     double precision, intent(inout) :: dest(:)
     integer, intent(in) :: inds(:)
     integer :: bmi_status
     type (c_ptr) src
     double precision, pointer :: src_flattened(:)
     integer :: i, n_elements

     select case(name)
     case default
        bmi_status = BMI_FAILURE
     end select
   end function snow17_get_at_indices_double

 ! Set new integer values.
  function snow17_set_int(this, name, src) result (bmi_status)
    class (bmi_snow17), intent(inout) :: this
    character (len=*), intent(in) :: name
    integer, intent(in) :: src(:)
    integer :: bmi_status

    !==================== UPDATE IMPLEMENTATION IF NECESSARY FOR INTEGER VARS =================

    select case(name)
!     case("model__identification_number")
!        this%model%id = src(1)
!        bmi_status = BMI_SUCCESS
    case default
       bmi_status = BMI_FAILURE
    end select
  end function snow17_set_int

  ! Set new real values.
  function snow17_set_float(this, name, src) result (bmi_status)
    class (bmi_snow17), intent(inout) :: this
    character (len=*), intent(in) :: name
    real, intent(in) :: src(:)
    integer :: bmi_status

    select case(name)
    case("precip")
       this%model%forcing%precip_comb = src(1)
       bmi_status = BMI_SUCCESS
    case("tair")
       this%model%forcing%tair_comb = src(1)
       bmi_status = BMI_SUCCESS
    case("precip_scf")
       this%model%forcing%precip_scf_comb = src(1)
       bmi_status = BMI_SUCCESS
    case("sneqv")
       this%model%modelvar%sneqv_comb = src(1)
       bmi_status = BMI_SUCCESS
    case("snowh")
       this%model%modelvar%snowh_comb = src(1)
       bmi_status = BMI_SUCCESS
    case("raim")
       this%model%modelvar%raim_comb = src(1)
       bmi_status = BMI_SUCCESS
    case default
       bmi_status = BMI_FAILURE
    end select
    ! NOTE, if vars are gridded, then use:
    ! this%model%temperature = reshape(src, [this%model%n_y, this%model%n_x])
  end function snow17_set_float

  ! Set new double values.
  function snow17_set_double(this, name, src) result (bmi_status)
    class (bmi_snow17), intent(inout) :: this
    character (len=*), intent(in) :: name
    double precision, intent(in) :: src(:)
    integer :: bmi_status

    !==================== UPDATE IMPLEMENTATION IF NECESSARY FOR DOUBLE VARS =================

    select case(name)
    case default
       bmi_status = BMI_FAILURE
    end select
  end function snow17_set_double

   ! Set integer values at particular locations.
   function snow17_set_at_indices_int(this, name, inds, src) &
        result (bmi_status)
     class (bmi_snow17), intent(inout) :: this
     character (len=*), intent(in) :: name
     integer, intent(in) :: inds(:)
     integer, intent(in) :: src(:)
     integer :: bmi_status
     type (c_ptr) dest
     integer, pointer :: dest_flattened(:)
     integer :: i

     select case(name)
     case default
        bmi_status = BMI_FAILURE
     end select
   end function snow17_set_at_indices_int

   ! Set real values at particular locations.
   function snow17_set_at_indices_float(this, name, inds, src) &
        result (bmi_status)
     class (bmi_snow17), intent(inout) :: this
     character (len=*), intent(in) :: name
     integer, intent(in) :: inds(:)
     real, intent(in) :: src(:)
     integer :: bmi_status
     type (c_ptr) dest
     real, pointer :: dest_flattened(:)
     integer :: i

     select case(name)
     case default
        bmi_status = BMI_FAILURE
     end select
   end function snow17_set_at_indices_float

   ! Set double values at particular locations.
   function snow17_set_at_indices_double(this, name, inds, src) &
        result (bmi_status)
     class (bmi_snow17), intent(inout) :: this
     character (len=*), intent(in) :: name
     integer, intent(in) :: inds(:)
     double precision, intent(in) :: src(:)
     integer :: bmi_status
     type (c_ptr) dest
     double precision, pointer :: dest_flattened(:)
     integer :: i

     select case(name)
     case default
        bmi_status = BMI_FAILURE
     end select
   end function snow17_set_at_indices_double

!   ! A non-BMI helper routine to advance the model by a fractional time step.
!   subroutine update_frac(this, time_frac)
!     class (bmi_snow17), intent(inout) :: this
!     double precision, intent(in) :: time_frac
!     real :: time_step
!
!     if (time_frac > 0.0) then
!        time_step = this%model%dt
!        this%model%dt = time_step*real(time_frac)
!        call advance_in_time(this%model)
!        this%model%dt = time_step
!     end if
!   end subroutine update_frac
!
!   ! A non-BMI procedure for model introspection.
!   subroutine print_model_info(this)
!     class (bmi_snow17), intent(in) :: this
!
!     call print_info(this%model)
!   end subroutine print_model_info
#ifdef NGEN_ACTIVE
  function register_bmi(this) result(bmi_status) bind(C, name="register_bmi")
   use, intrinsic:: iso_c_binding, only: c_ptr, c_loc, c_int
   use iso_c_bmif_2_0
   implicit none
   type(c_ptr) :: this ! If not value, then from the C perspective `this` is a void**
   integer(kind=c_int) :: bmi_status
   !Create the model instance to use
   type(bmi_snow17), pointer :: bmi_model
   !Create a simple pointer wrapper
   type(box), pointer :: bmi_box

   !allocate model
   allocate(bmi_snow17::bmi_model)
   !allocate the pointer box
   allocate(bmi_box)

   !associate the wrapper pointer the created model instance
   bmi_box%ptr => bmi_model

   if( .not. associated( bmi_box ) .or. .not. associated( bmi_box%ptr ) ) then
    bmi_status = BMI_FAILURE
   else
    !Return the pointer to box
    this = c_loc(bmi_box)
    bmi_status = BMI_SUCCESS
   endif
 end function register_bmi
#endif

end module bmi_snow17_module
