cmake_minimum_required (VERSION 3.0.0)
project(TortureTester)


SET(DEPS_SRC 
	deps/getopt.c
	deps/getopt.h
	deps/sratom.c
	deps/sratom.h
	deps/sord.c
	deps/sord.h
	deps/serd.c
	deps/serd.h
	deps/lilv.h
	deps/lilv.c
)

add_executable(TortureTester 
	src/input_buffers.cc
	src/input_profile.cc
	src/ladspa_plugin.cc
	src/lib_loading.cc
	src/log.cc
	src/lv2_plugin.cc
	src/main.cc
	src/plugin.cc
	src/tests.cc
	src/uri_map.cc
	src/tools.cc
	${DEPS_SRC} 
	#src/vst_plugin.cc
${TORTURE_TESTER_SRC})
target_include_directories(TortureTester PUBLIC src deps deps/lv2 deps/sord deps/serd deps/lilv)

if (WIN32)
	target_compile_definitions(TortureTester PUBLIC __WIN32__)
endif(WIN32)