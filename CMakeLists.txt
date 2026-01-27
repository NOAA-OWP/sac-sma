cmake_minimum_required(VERSION 3.12)
enable_language( Fortran )
include(CMakeDependentOption)

# Perhaps in the future set/append to CMAKE_MODULE_PATH and create a cmake directory with helper cmake stuff
# list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake/")

project(sac-sma VERSION 1.0.1 DESCRIPTION "OWP NextGen Sac-SMA Implementation")

# Do this so we can default to ../iso_c_fortran_bmi for bindings if that directory exists
if (IS_DIRECTORY ${CMAKE_CURRENT_LIST_DIR}/../iso_c_fortran_bmi)
    set(DEFAULT_ISO_C_BMI_DIR_VAL ${CMAKE_CURRENT_LIST_DIR}/../iso_c_fortran_bmi)
else ()
    set(DEFAULT_ISO_C_BMI_DIR_VAL OFF)
endif ()

option(ISO_C_FORTRAN_BMI_PATH  "Path to NextGen middleware for ISO C Fortran BMI bindings" ${DEFAULT_ISO_C_BMI_DIR_VAL})

# Syntax: cmake_dependent_option(<option> "<help_text>" <value> <depends> <force>)
cmake_dependent_option(BMI_SHARED_LIB "Support for building BMI module shared library" ON "ISO_C_FORTRAN_BMI_PATH" OFF)

if (${BMI_SHARED_LIB} AND NOT IS_DIRECTORY ${ISO_C_FORTRAN_BMI_PATH})
    message(FATAL_ERROR "Provided CMake option 'ISO_C_FORTRAN_BMI_PATH' is not a valid directory!")
endif ()

# TODO: (later) need to figure out the proper analogous compiler settings for -Wall in Intel and nv compilers
if ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "Intel|IntelLLVM")
    add_compile_options(-cpp -fp-model=strict)
elseif ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "GNU")
    add_compile_options(-cpp -Wall -fcheck=bounds -ffree-line-length-none -frounding-math -fno-fast-math)
elseif ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "NVIDIA" OR "${CMAKE_Fortran_COMPILER_ID}" MATCHES "NVHPC")
	add_compile_options(${SAC_LIB_NAME_CMAKE} -Kieee -Mbackslash -Mpreprocess)
endif ()

#### Add variables for individual libraries that are used withing the *.pc.in files
# sac-sma
set(SAC_LIB_NAME_CMAKE sacbmi)
set(SAC_STANDALONE_EXE_NAME sac)
set(SAC_LIB_DESC_CMAKE "External Sac-SMA Shared Library")
set(PARENT_SOURCE_DIR ${CMAKE_CURRENT_LIST_DIR}/src/)
set(BMI_SOURCES_DIR ${PARENT_SOURCE_DIR}/bmi/)
set(DRIVER_SOURCE_DIR ${PARENT_SOURCE_DIR}/driver/)
set(MODEL_SOURCE_DIR ${PARENT_SOURCE_DIR}/sac/)
set(SHARE_SOURCE_DIR ${PARENT_SOURCE_DIR}/share/)

# Create variables with the source files needed in different contexts
set(BMI_SOURCES "src/bmi/bmi.f90" "src/bmi/bmi_sac.f90")
set(MODEL_SOURCES "src/sac/ex_sac1.f" "src/sac/sac1.f")
set(DRIVER_SOURCES "src/driver/driver_bmi.f90")
set(SHARE_SOURCES "src/share/constants.f90" "src/share/dateTimeUtilsModule.f90" "src/share/derivedType.f90"
	"src/share/forcingType.f90" "src/share/ioModule.f90" "src/share/modelVarType.f90"
	"src/share/namelistModule.f90" "src/share/nrtype.f90" "src/share/parametersType.f90"
	"src/share/runInfoType.f90" "src/share/runSac.f90"
)

add_executable(${SAC_STANDALONE_EXE_NAME} ${MODEL_SOURCES} ${BMI_SOURCES} ${DRIVER_SOURCES} ${SHARE_SOURCES})

if (BMI_SHARED_LIB AND WIN32)
    message(FATAL_ERROR "Currently shared library target is not supported on Windows platforms.")
elseif (BMI_SHARED_LIB)
    add_subdirectory(${ISO_C_FORTRAN_BMI_PATH} ${CMAKE_BINARY_DIR}/iso_c_bmi)
    add_library(${SAC_LIB_NAME_CMAKE} SHARED ${BMI_SOURCES} ${MODEL_SOURCES} ${DRIVER_SOURCES} ${SHARE_SOURCES})
    target_compile_definitions(${SAC_LIB_NAME_CMAKE} PUBLIC BMI_ACTIVE NGEN_FORCING_ACTIVE NGEN_OUTPUT_ACTIVE NGEN_ACTIVE)
    target_compile_options(${SAC_LIB_NAME_CMAKE} PUBLIC -fPIC)
    target_link_libraries(${SAC_LIB_NAME_CMAKE} PUBLIC iso_c_bmi)
    set_target_properties(${SAC_LIB_NAME_CMAKE} PROPERTIES VERSION ${PROJECT_VERSION})

#    include(GNUInstallDirs)
#
#    install(TARGETS ${SAC_LIB_NAME_CMAKE}
#            LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
#            PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})
endif ()
