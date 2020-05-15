cmake_minimum_required (VERSION 3.0.0)
project(TortureTester)

option(TORTURE_TESTER_INSTALL "Installs TortureTester Binary to System" ON)

SET(DEPS_SRC 
	${CMAKE_CURRENT_LIST_DIR}/deps/getopt.c
	${CMAKE_CURRENT_LIST_DIR}/deps/getopt.h
	${CMAKE_CURRENT_LIST_DIR}/deps/sratom.c
	${CMAKE_CURRENT_LIST_DIR}/deps/sratom.h
	${CMAKE_CURRENT_LIST_DIR}/deps/sord.c
	${CMAKE_CURRENT_LIST_DIR}/deps/sord.h
	${CMAKE_CURRENT_LIST_DIR}/deps/serd.c
	${CMAKE_CURRENT_LIST_DIR}/deps/serd.h
	${CMAKE_CURRENT_LIST_DIR}/deps/lilv.h
	${CMAKE_CURRENT_LIST_DIR}/deps/lilv.c
)

add_executable(TortureTester 
	${CMAKE_CURRENT_LIST_DIR}/src/input_buffers.cc
	${CMAKE_CURRENT_LIST_DIR}/src/input_profile.cc
	${CMAKE_CURRENT_LIST_DIR}/src/ladspa_plugin.cc
	${CMAKE_CURRENT_LIST_DIR}/src/lib_loading.cc
	${CMAKE_CURRENT_LIST_DIR}/src/log.cc
	${CMAKE_CURRENT_LIST_DIR}/src/lv2_plugin.cc
	${CMAKE_CURRENT_LIST_DIR}/src/main.cc
	${CMAKE_CURRENT_LIST_DIR}/src/plugin.cc
	${CMAKE_CURRENT_LIST_DIR}/src/tests.cc
	${CMAKE_CURRENT_LIST_DIR}/src/uri_map.cc
	${CMAKE_CURRENT_LIST_DIR}/src/tools.cc
	${DEPS_SRC} 
	#src/vst_plugin.cc
${TORTURE_TESTER_SRC})
target_include_directories(TortureTester PRIVATE 
${CMAKE_CURRENT_LIST_DIR}/src 
${CMAKE_CURRENT_LIST_DIR}/deps 
${CMAKE_CURRENT_LIST_DIR}/deps/lv2 )

if (WIN32)
	target_compile_definitions(TortureTester PUBLIC __WIN32__)
else()
        target_link_libraries(TortureTester dl)
endif(WIN32)

if(TORTURE_TESTER_INSTALL)
install(TARGETS TortureTester EXPORT TortureTestingBin DESTINATION bin)
install(EXPORT TortureTestingBin DESTINATION bin/TortureTester)
endif(TORTURE_TESTER_INSTALL)
