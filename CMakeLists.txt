cmake_minimum_required (VERSION 3.0.0)
project(TortureTester)
SET(LILV_SRC 
	deps/lilv/collections.c
	deps/lilv/instance.c
	deps/lilv/lib.c
	deps/lilv/node.c
	deps/lilv/plugin.c
	deps/lilv/pluginclass.c
	deps/lilv/port.c
	deps/lilv/query.c
	deps/lilv/scalepoint.c
	deps/lilv/state.c
	deps/lilv/ui.c
	deps/lilv/util.c
	deps/lilv/world.c
	deps/lilv/zix/tree.c
)
SET(SERD_SRC  
	deps/serd/byte_source.c
	deps/serd/env.c
	deps/serd/n3.c
	deps/serd/node.c
	deps/serd/reader.c
	deps/serd/string.c
	deps/serd/uri.c
	deps/serd/writer.c
)
SET(SORD_SRC
	deps/sord/sord.c
	deps/sord/syntax.c
	#deps/sord/zix/btree.c
	#deps/sord/zix/digest.c
	#deps/sord/zix/hash.c
)

SET(SRATOM_SRC
	deps/sratom/sratom.c
)

SET(DEPS_SRC 
	deps/getopt.c
	${LILV_SRC} ${SERD_SRC} ${SORD_SRC} ${SRATOM_SRC}
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