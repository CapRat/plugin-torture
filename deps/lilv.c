#include "lilv.h"

#include "sord.h"
#include "serd.h"
#include "sratom.h"
#include "lv2/core/lv2.h"
#include "lv2/ui/ui.h"
#include "lv2/atom/atom.h"
#include "lv2/event/event.h"
#include "lv2/atom/forge.h"
#include "lv2/presets/presets.h"
#include "lv2/state/state.h"
#include "lv2/urid/urid.h"

#ifdef LILV_DYN_MANIFEST
#    include "lv2/dynmanifest/dynmanifest.h"
#    include <dlfcn.h>
#endif

#include <math.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/types.h>

#ifdef _WIN32
#    include <windows.h>
#    include <direct.h>
#    include <stdio.h>
#    define dlopen(path, flags) LoadLibrary(path)
#    define dlclose(lib)        FreeLibrary((HMODULE)lib)
#    define unlink(path)        _unlink(path)
#    define rmdir(path)         _rmdir(path)
#    ifdef _MSC_VER
#        define __func__ __FUNCTION__
#        ifndef snprintf
#            define snprintf _snprintf
#        endif
#    endif
#ifndef INFINITY
#    define INFINITY DBL_MAX + DBL_MAX
#endif
#ifndef NAN
#    define NAN INFINITY - INFINITY
#endif
static inline const char* dlerror(void) { return "Unknown error"; }
#else
#    include <dlfcn.h>
#    include <unistd.h>
#endif


/*
 *
 * Types
 *
 */

typedef struct LilvSpecImpl LilvSpec;

typedef void LilvCollection;

struct LilvPortImpl {
	LilvNode* node;     ///< RDF node
	uint32_t   index;    ///< lv2:index
	LilvNode* symbol;   ///< lv2:symbol
	LilvNodes* classes;  ///< rdf:type
};

struct LilvSpecImpl {
	SordNode* spec;
	SordNode* bundle;
	LilvNodes* data_uris;
	struct LilvSpecImpl* next;
};

/**
   Header of an LilvPlugin, LilvPluginClass, or LilvUI.
   Any of these structs may be safely casted to LilvHeader, which is used to
   implement collections using the same comparator.
*/
struct LilvHeader {
	LilvWorld* world;
	LilvNode* uri;
};

#ifdef LILV_DYN_MANIFEST
typedef struct {
	LilvNode* bundle;
	void* lib;
	LV2_Dyn_Manifest_Handle handle;
	uint32_t                refs;
} LilvDynManifest;
#endif

typedef struct {
	LilvWorld* world;
	LilvNode* uri;
	char* bundle_path;
	void* lib;
	LV2_Descriptor_Function   lv2_descriptor;
	const LV2_Lib_Descriptor* desc;
	uint32_t                  refs;
} LilvLib;

struct LilvPluginImpl {
	LilvWorld* world;
	LilvNode* plugin_uri;
	LilvNode* bundle_uri;  ///< Bundle plugin was loaded from
	LilvNode* binary_uri;  ///< lv2:binary
#ifdef LILV_DYN_MANIFEST
	LilvDynManifest* dynmanifest;
#endif
	const LilvPluginClass* plugin_class;
	LilvNodes* data_uris;  ///< rdfs::seeAlso
	LilvPort** ports;
	uint32_t               num_ports;
	bool                   loaded;
	bool                   parse_errors;
	bool                   replaced;
};

struct LilvPluginClassImpl {
	LilvWorld* world;
	LilvNode* uri;
	LilvNode* parent_uri;
	LilvNode* label;
};

struct LilvInstancePimpl {
	LilvWorld* world;
	LilvLib* lib;
};

typedef struct {
	bool  dyn_manifest;
	bool  filter_language;
	char* lv2_path;
} LilvOptions;

struct LilvWorldImpl {
	SordWorld* world;
	SordModel* model;
	SerdReader* reader;
	unsigned           n_read_files;
	LilvPluginClass* lv2_plugin_class;
	LilvPluginClasses* plugin_classes;
	LilvSpec* specs;
	LilvPlugins* plugins;
	LilvPlugins* zombies;
	LilvNodes* loaded_files;
	ZixTree* libs;
	struct {
		SordNode* dc_replaces;
		SordNode* dman_DynManifest;
		SordNode* doap_name;
		SordNode* lv2_Plugin;
		SordNode* lv2_Specification;
		SordNode* lv2_appliesTo;
		SordNode* lv2_binary;
		SordNode* lv2_default;
		SordNode* lv2_designation;
		SordNode* lv2_extensionData;
		SordNode* lv2_index;
		SordNode* lv2_latency;
		SordNode* lv2_maximum;
		SordNode* lv2_microVersion;
		SordNode* lv2_minimum;
		SordNode* lv2_minorVersion;
		SordNode* lv2_name;
		SordNode* lv2_optionalFeature;
		SordNode* lv2_port;
		SordNode* lv2_portProperty;
		SordNode* lv2_reportsLatency;
		SordNode* lv2_requiredFeature;
		SordNode* lv2_symbol;
		SordNode* lv2_prototype;
		SordNode* owl_Ontology;
		SordNode* pset_value;
		SordNode* rdf_a;
		SordNode* rdf_value;
		SordNode* rdfs_Class;
		SordNode* rdfs_label;
		SordNode* rdfs_seeAlso;
		SordNode* rdfs_subClassOf;
		SordNode* xsd_base64Binary;
		SordNode* xsd_boolean;
		SordNode* xsd_decimal;
		SordNode* xsd_double;
		SordNode* xsd_integer;
		SordNode* null_uri;
	} uris;
	LilvOptions opt;
};

typedef enum {
	LILV_VALUE_URI,
	LILV_VALUE_STRING,
	LILV_VALUE_INT,
	LILV_VALUE_FLOAT,
	LILV_VALUE_BOOL,
	LILV_VALUE_BLANK,
	LILV_VALUE_BLOB
} LilvNodeType;

struct LilvNodeImpl {
	LilvWorld* world;
	SordNode* node;
	LilvNodeType type;
	union {
		int   int_val;
		float float_val;
		bool  bool_val;
	} val;
};

struct LilvScalePointImpl {
	LilvNode* value;
	LilvNode* label;
};

struct LilvUIImpl {
	LilvWorld* world;
	LilvNode* uri;
	LilvNode* bundle_uri;
	LilvNode* binary_uri;
	LilvNodes* classes;
};

typedef struct LilvVersion {
	int minor;
	int micro;
} LilvVersion;

/*
 *
 * Functions
 *
 */

LilvPort* lilv_port_new(LilvWorld* world,
	const SordNode* node,
	uint32_t        index,
	const char* symbol);
void      lilv_port_free(const LilvPlugin* plugin, LilvPort* port);

LilvPlugin* lilv_plugin_new(LilvWorld* world,
	LilvNode* uri,
	LilvNode* bundle_uri);
void        lilv_plugin_clear(LilvPlugin* plugin, LilvNode* bundle_uri);
void        lilv_plugin_load_if_necessary(const LilvPlugin* plugin);
void        lilv_plugin_free(LilvPlugin* plugin);
LilvNode* lilv_plugin_get_unique(const LilvPlugin* plugin,
	const SordNode* subject,
	const SordNode* predicate);

void      lilv_collection_free(LilvCollection* collection);
unsigned  lilv_collection_size(const LilvCollection* collection);
LilvIter* lilv_collection_begin(const LilvCollection* collection);
void* lilv_collection_get(const LilvCollection* collection,
	const LilvIter* i);

LilvPluginClass* lilv_plugin_class_new(LilvWorld* world,
	const SordNode* parent_node,
	const SordNode* uri,
	const char* label);

void lilv_plugin_class_free(LilvPluginClass* plugin_class);

LilvLib*
lilv_lib_open(LilvWorld* world,
	const LilvNode* uri,
	const char* bundle_path,
	const LV2_Feature* const* features);

const LV2_Descriptor* lilv_lib_get_plugin(LilvLib* lib, uint32_t index);
void                  lilv_lib_close(LilvLib* lib);

LilvNodes* lilv_nodes_new(void);
LilvPlugins* lilv_plugins_new(void);
LilvScalePoints* lilv_scale_points_new(void);
LilvPluginClasses* lilv_plugin_classes_new(void);
LilvUIs* lilv_uis_new(void);

LilvNode* lilv_world_get_manifest_uri(LilvWorld* world,
	const LilvNode* bundle_uri);

const uint8_t* lilv_world_blank_node_prefix(LilvWorld* world);

SerdStatus lilv_world_load_file(LilvWorld* world,
	SerdReader* reader,
	const LilvNode* uri);

SerdStatus
lilv_world_load_graph(LilvWorld* world,
	SordNode* graph,
	const LilvNode* uri);

LilvUI* lilv_ui_new(LilvWorld* world,
	LilvNode* uri,
	LilvNode* type_uri,
	LilvNode* binary_uri);

void lilv_ui_free(LilvUI* ui);

LilvNode* lilv_node_new(LilvWorld* world, LilvNodeType type, const char* str);
LilvNode* lilv_node_new_from_node(LilvWorld* world, const SordNode* node);

int lilv_header_compare_by_uri(const void* a, const void* b, void* user_data);
int lilv_lib_compare(const void* a, const void* b, void* user_data);

int lilv_ptr_cmp(const void* a, const void* b, void* user_data);
int lilv_resource_node_cmp(const void* a, const void* b, void* user_data);

static inline int
lilv_version_cmp(const LilvVersion* a, const LilvVersion* b)
{
	if (a->minor == b->minor && a->micro == b->micro) {
		return 0;
	}
	else if ((a->minor < b->minor)
		|| (a->minor == b->minor && a->micro < b->micro)) {
		return -1;
	}
	else {
		return 1;
	}
}

struct LilvHeader*
	lilv_collection_get_by_uri(const ZixTree* seq, const LilvNode* uri);

LilvScalePoint* lilv_scale_point_new(LilvNode* value, LilvNode* label);
void            lilv_scale_point_free(LilvScalePoint* point);

SordIter*
lilv_world_query_internal(LilvWorld* world,
	const SordNode* subject,
	const SordNode* predicate,
	const SordNode* object);

bool
lilv_world_ask_internal(LilvWorld* world,
	const SordNode* subject,
	const SordNode* predicate,
	const SordNode* object);

LilvNodes*
lilv_world_find_nodes_internal(LilvWorld* world,
	const SordNode* subject,
	const SordNode* predicate,
	const SordNode* object);

SordModel*
lilv_world_filter_model(LilvWorld* world,
	SordModel* model,
	const SordNode* subject,
	const SordNode* predicate,
	const SordNode* object,
	const SordNode* graph);

#define FOREACH_MATCH(iter) \
	for (; !sord_iter_end(iter); sord_iter_next(iter))

LilvNodes* lilv_nodes_from_stream_objects(LilvWorld* world,
	SordIter* stream,
	SordQuadIndex field);

char* lilv_strjoin(const char* first, ...);
char* lilv_strdup(const char* str);
char* lilv_get_lang(void);
char* lilv_expand(const char* path);
char* lilv_dirname(const char* path);
char* lilv_dir_path(const char* path);
int    lilv_copy_file(const char* src, const char* dst);
bool   lilv_path_exists(const char* path, const void* ignored);
char* lilv_path_absolute(const char* path);
bool   lilv_path_is_absolute(const char* path);
char* lilv_get_latest_copy(const char* path, const char* copy_path);
char* lilv_path_relative_to(const char* path, const char* base);
bool   lilv_path_is_child(const char* path, const char* dir);
int    lilv_flock(FILE* file, bool lock);
char* lilv_realpath(const char* path);
int    lilv_symlink(const char* oldpath, const char* newpath);
int    lilv_mkdir_p(const char* dir_path);
char* lilv_path_join(const char* a, const char* b);
bool   lilv_file_equals(const char* a_path, const char* b_path);

char*
lilv_find_free_path(const char* in_path,
	bool (*exists)(const char*, const void*),
	const void* user_data);

void
lilv_dir_for_each(const char* path,
	void* data,
	void (*f)(const char* path, const char* name, void* data));

typedef void (*LilvVoidFunc)(void);

/** dlsym wrapper to return a function pointer (without annoying warning) */
static inline LilvVoidFunc
lilv_dlfunc(void* handle, const char* symbol)
{
#ifdef _WIN32
	return (LilvVoidFunc)GetProcAddress((HMODULE)handle, symbol);
#else
	typedef LilvVoidFunc(*VoidFuncGetter)(void*, const char*);
	VoidFuncGetter dlfunc = (VoidFuncGetter)dlsym;
	return dlfunc(handle, symbol);
#endif
}

#ifdef LILV_DYN_MANIFEST
static const LV2_Feature* const dman_features = { NULL };

void lilv_dynmanifest_free(LilvDynManifest* dynmanifest);
#endif

#define LILV_ERROR(str)       fprintf(stderr, "%s(): error: " str, \
                                      __func__)
#define LILV_ERRORF(fmt, ...) fprintf(stderr, "%s(): error: " fmt, \
                                      __func__, __VA_ARGS__)
#define LILV_WARN(str)        fprintf(stderr, "%s(): warning: " str, \
                                      __func__)
#define LILV_WARNF(fmt, ...)  fprintf(stderr, "%s(): warning: " fmt, \
                                      __func__, __VA_ARGS__)
#define LILV_NOTE(str)        fprintf(stderr, "%s(): note: " str, \
                                      __func__)
#define LILV_NOTEF(fmt, ...)  fprintf(stderr, "%s(): note: " fmt, \
                                      __func__, __VA_ARGS__)



int
lilv_ptr_cmp(const void* a, const void* b, void* user_data)
{
	return (intptr_t)a - (intptr_t)b;
}

int
lilv_resource_node_cmp(const void* a, const void* b, void* user_data)
{
	const SordNode* an = ((const LilvNode*)a)->node;
	const SordNode* bn = ((const LilvNode*)b)->node;
	return (intptr_t)an - (intptr_t)bn;
}

/* Generic collection functions */

static inline LilvCollection*
lilv_collection_new(ZixComparator cmp, ZixDestroyFunc destructor)
{
	return zix_tree_new(false, cmp, NULL, destructor);
}

void
lilv_collection_free(LilvCollection* collection)
{
	if (collection) {
		zix_tree_free((ZixTree*)collection);
	}
}

unsigned
lilv_collection_size(const LilvCollection* collection)
{
	return (collection ? zix_tree_size((const ZixTree*)collection) : 0);
}

LilvIter*
lilv_collection_begin(const LilvCollection* collection)
{
	return collection ? (LilvIter*)zix_tree_begin((ZixTree*)collection) : NULL;
}

void*
lilv_collection_get(const LilvCollection* collection,
	const LilvIter* i)
{
	return zix_tree_get((const ZixTreeIter*)i);
}

/* Constructors */

LilvScalePoints*
lilv_scale_points_new(void)
{
	return lilv_collection_new(lilv_ptr_cmp,
		(ZixDestroyFunc)lilv_scale_point_free);
}

LilvNodes*
lilv_nodes_new(void)
{
	return lilv_collection_new(lilv_ptr_cmp,
		(ZixDestroyFunc)lilv_node_free);
}

LilvUIs*
lilv_uis_new(void)
{
	return lilv_collection_new(lilv_header_compare_by_uri,
		(ZixDestroyFunc)lilv_ui_free);
}

LilvPluginClasses*
lilv_plugin_classes_new(void)
{
	return lilv_collection_new(lilv_header_compare_by_uri,
		(ZixDestroyFunc)lilv_plugin_class_free);
}

/* URI based accessors (for collections of things with URIs) */

const LilvPluginClass*
lilv_plugin_classes_get_by_uri(const LilvPluginClasses* classes,
	const LilvNode* uri)
{
	return (LilvPluginClass*)lilv_collection_get_by_uri(
		(const ZixTree*)classes, uri);
}

const LilvUI*
lilv_uis_get_by_uri(const LilvUIs* uis, const LilvNode* uri)
{
	return (LilvUI*)lilv_collection_get_by_uri((const ZixTree*)uis, uri);
}

/* Plugins */

LilvPlugins*
lilv_plugins_new(void)
{
	return lilv_collection_new(lilv_header_compare_by_uri, NULL);
}

const LilvPlugin*
lilv_plugins_get_by_uri(const LilvPlugins* plugins, const LilvNode* uri)
{
	return (LilvPlugin*)lilv_collection_get_by_uri(
		(const ZixTree*)plugins, uri);
}

/* Nodes */

bool
lilv_nodes_contains(const LilvNodes* nodes, const LilvNode* value)
{
	LILV_FOREACH(nodes, i, nodes) {
		if (lilv_node_equals(lilv_nodes_get(nodes, i), value)) {
			return true;
		}
	}

	return false;
}

LilvNodes*
lilv_nodes_merge(const LilvNodes* a, const LilvNodes* b)
{
	LilvNodes* result = lilv_nodes_new();

	LILV_FOREACH(nodes, i, a)
		zix_tree_insert((ZixTree*)result,
			lilv_node_duplicate(lilv_nodes_get(a, i)),
			NULL);

	LILV_FOREACH(nodes, i, b)
		zix_tree_insert((ZixTree*)result,
			lilv_node_duplicate(lilv_nodes_get(b, i)),
			NULL);

	return result;
}

/* Iterator */

#define LILV_COLLECTION_IMPL(prefix, CT, ET) \
\
unsigned \
prefix##_size(const CT* collection) { \
	return lilv_collection_size(collection); \
} \
\
\
LilvIter* \
prefix##_begin(const CT* collection) { \
	return lilv_collection_begin(collection); \
} \
\
\
const ET* \
prefix##_get(const CT* collection, LilvIter* i) { \
	return (ET*)lilv_collection_get(collection, i); \
} \
\
\
LilvIter* \
prefix##_next(const CT* collection, LilvIter* i) { \
	return zix_tree_iter_next((ZixTreeIter*)i); \
} \
\
\
bool \
prefix##_is_end(const CT* collection, LilvIter* i) { \
	return zix_tree_iter_is_end((ZixTreeIter*)i); \
}

LILV_COLLECTION_IMPL(lilv_plugin_classes, LilvPluginClasses, LilvPluginClass)
LILV_COLLECTION_IMPL(lilv_scale_points, LilvScalePoints, LilvScalePoint)
LILV_COLLECTION_IMPL(lilv_uis, LilvUIs, LilvUI)
LILV_COLLECTION_IMPL(lilv_nodes, LilvNodes, LilvNode)
LILV_COLLECTION_IMPL(lilv_plugins, LilvPlugins, LilvPlugin)

void
lilv_plugin_classes_free(LilvPluginClasses* collection) {
	lilv_collection_free(collection);
}

void
lilv_scale_points_free(LilvScalePoints* collection) {
	lilv_collection_free(collection);
}

void
lilv_uis_free(LilvUIs* collection) {
	lilv_collection_free(collection);
}

void
lilv_nodes_free(LilvNodes* collection) {
	lilv_collection_free(collection);
}

LilvNode*
lilv_nodes_get_first(const LilvNodes* collection) {
	return (LilvNode*)lilv_collection_get(collection,
		lilv_collection_begin(collection));
}

LilvInstance*
lilv_plugin_instantiate(const LilvPlugin* plugin,
	double                   sample_rate,
	const LV2_Feature* const* features)
{
	lilv_plugin_load_if_necessary(plugin);
	if (plugin->parse_errors) {
		return NULL;
	}

	LilvInstance* result = NULL;
	const LilvNode* const lib_uri = lilv_plugin_get_library_uri(plugin);
	const LilvNode* const bundle_uri = lilv_plugin_get_bundle_uri(plugin);
	if (!lib_uri || !bundle_uri) {
		return NULL;
	}

	char* const bundle_path = lilv_file_uri_parse(
		lilv_node_as_uri(bundle_uri), NULL);

	LilvLib* lib = lilv_lib_open(plugin->world, lib_uri, bundle_path, features);
	if (!lib) {
		serd_free(bundle_path);
		return NULL;
	}

	const LV2_Feature** local_features = NULL;
	if (features == NULL) {
		local_features = (const LV2_Feature**)malloc(sizeof(LV2_Feature*));
		local_features[0] = NULL;
	}

	// Search for plugin by URI
	for (uint32_t i = 0; true; ++i) {
		const LV2_Descriptor* ld = lilv_lib_get_plugin(lib, i);
		if (!ld) {
			LILV_ERRORF("No plugin <%s> in <%s>\n",
				lilv_node_as_uri(lilv_plugin_get_uri(plugin)),
				lilv_node_as_uri(lib_uri));
			lilv_lib_close(lib);
			break;  // return NULL
		}

		if (!strcmp(ld->URI, lilv_node_as_uri(lilv_plugin_get_uri(plugin)))) {
			// Create LilvInstance to return
			result = (LilvInstance*)malloc(sizeof(LilvInstance));
			result->lv2_descriptor = ld;
			result->lv2_handle = ld->instantiate(
				ld, sample_rate, bundle_path,
				(features) ? features : local_features);
			result->pimpl = lib;
			break;
		}
	}

	free(local_features);
	serd_free(bundle_path);

	if (result) {
		if (result->lv2_handle == NULL) {
			// Failed to instantiate
			free(result);
			lilv_lib_close(lib);
			return NULL;
		}

		// "Connect" all ports to NULL (catches bugs)
		for (uint32_t i = 0; i < lilv_plugin_get_num_ports(plugin); ++i) {
			result->lv2_descriptor->connect_port(result->lv2_handle, i, NULL);
		}
	}

	return result;
}

void
lilv_instance_free(LilvInstance* instance)
{
	if (!instance) {
		return;
	}

	instance->lv2_descriptor->cleanup(instance->lv2_handle);
	instance->lv2_descriptor = NULL;
	lilv_lib_close((LilvLib*)instance->pimpl);
	instance->pimpl = NULL;
	free(instance);
}


LilvLib*
lilv_lib_open(LilvWorld* world,
	const LilvNode* uri,
	const char* bundle_path,
	const LV2_Feature* const* features)
{
	ZixTreeIter* i = NULL;
	const LilvLib key = {
		world, (LilvNode*)uri, (char*)bundle_path, NULL, NULL, NULL, 0
	};
	if (!zix_tree_find(world->libs, &key, &i)) {
		LilvLib* llib = (LilvLib*)zix_tree_get(i);
		++llib->refs;
		return llib;
	}

	const char* const lib_uri = lilv_node_as_uri(uri);
	char* const       lib_path = (char*)serd_file_uri_parse(
		(const uint8_t*)lib_uri, NULL);
	if (!lib_path) {
		return NULL;
	}

	dlerror();
	void* lib = dlopen(lib_path, RTLD_NOW);
	if (!lib) {
		LILV_ERRORF("Failed to open library %s (%s)\n", lib_path, dlerror());
		serd_free(lib_path);
		return NULL;
	}

	LV2_Descriptor_Function df = (LV2_Descriptor_Function)
		lilv_dlfunc(lib, "lv2_descriptor");

	LV2_Lib_Descriptor_Function ldf = (LV2_Lib_Descriptor_Function)
		lilv_dlfunc(lib, "lv2_lib_descriptor");

	const LV2_Lib_Descriptor* desc = NULL;
	if (ldf) {
		desc = ldf(bundle_path, features);
		if (!desc) {
			LILV_ERRORF("Call to %s:lv2_lib_descriptor failed\n", lib_path);
			dlclose(lib);
			serd_free(lib_path);
			return NULL;
		}
	}
	else if (!df) {
		LILV_ERRORF("No `lv2_descriptor' or `lv2_lib_descriptor' in %s\n",
			lib_path);
		dlclose(lib);
		serd_free(lib_path);
		return NULL;
	}
	serd_free(lib_path);

	LilvLib* llib = (LilvLib*)malloc(sizeof(LilvLib));
	llib->world = world;
	llib->uri = lilv_node_duplicate(uri);
	llib->bundle_path = lilv_strdup(bundle_path);
	llib->lib = lib;
	llib->lv2_descriptor = df;
	llib->desc = desc;
	llib->refs = 1;

	zix_tree_insert(world->libs, llib, NULL);
	return llib;
}

const LV2_Descriptor*
lilv_lib_get_plugin(LilvLib* lib, uint32_t index)
{
	if (lib->lv2_descriptor) {
		return lib->lv2_descriptor(index);
	}
	else if (lib->desc) {
		return lib->desc->get_plugin(lib->desc->handle, index);
	}
	return NULL;
}

void
lilv_lib_close(LilvLib* lib)
{
	if (--lib->refs == 0) {
		dlclose(lib->lib);

		ZixTreeIter* i = NULL;
		if (lib->world->libs && !zix_tree_find(lib->world->libs, lib, &i)) {
			zix_tree_remove(lib->world->libs, i);
		}

		lilv_node_free(lib->uri);
		free(lib->bundle_path);
		free(lib);
	}
}


static void
lilv_node_set_numerics_from_string(LilvNode* val)
{
	const char* str = (const char*)sord_node_get_string(val->node);

	switch (val->type) {
	case LILV_VALUE_URI:
	case LILV_VALUE_BLANK:
	case LILV_VALUE_STRING:
	case LILV_VALUE_BLOB:
		break;
	case LILV_VALUE_INT:
		val->val.int_val = strtol(str, NULL, 10);
		break;
	case LILV_VALUE_FLOAT:
		val->val.float_val = serd_strtod(str, NULL);
		break;
	case LILV_VALUE_BOOL:
		val->val.bool_val = !strcmp(str, "true");
		break;
	}
}

/** Note that if `type` is numeric or boolean, the returned value is corrupt
 * until lilv_node_set_numerics_from_string is called.  It is not
 * automatically called from here to avoid overhead and imprecision when the
 * exact string value is known.
 */
LilvNode*
lilv_node_new(LilvWorld* world, LilvNodeType type, const char* str)
{
	LilvNode* val = (LilvNode*)malloc(sizeof(LilvNode));
	val->world = world;
	val->type = type;

	const uint8_t* ustr = (const uint8_t*)str;
	switch (type) {
	case LILV_VALUE_URI:
		val->node = sord_new_uri(world->world, ustr);
		break;
	case LILV_VALUE_BLANK:
		val->node = sord_new_blank(world->world, ustr);
		break;
	case LILV_VALUE_STRING:
		val->node = sord_new_literal(world->world, NULL, ustr, NULL);
		break;
	case LILV_VALUE_INT:
		val->node = sord_new_literal(
			world->world, world->uris.xsd_integer, ustr, NULL);
		break;
	case LILV_VALUE_FLOAT:
		val->node = sord_new_literal(
			world->world, world->uris.xsd_decimal, ustr, NULL);
		break;
	case LILV_VALUE_BOOL:
		val->node = sord_new_literal(
			world->world, world->uris.xsd_boolean, ustr, NULL);
		break;
	case LILV_VALUE_BLOB:
		val->node = sord_new_literal(
			world->world, world->uris.xsd_base64Binary, ustr, NULL);
		break;
	}

	if (!val->node) {
		free(val);
		return NULL;
	}

	return val;
}

/** Create a new LilvNode from `node`, or return NULL if impossible */
LilvNode*
lilv_node_new_from_node(LilvWorld* world, const SordNode* node)
{
	if (!node) {
		return NULL;
	}

	LilvNode* result = NULL;
	SordNode* datatype_uri = NULL;
	LilvNodeType type = LILV_VALUE_STRING;

	switch (sord_node_get_type(node)) {
	case SORD_URI:
		result = (LilvNode*)malloc(sizeof(LilvNode));
		result->world = world;
		result->type = LILV_VALUE_URI;
		result->node = sord_node_copy(node);
		break;
	case SORD_BLANK:
		result = (LilvNode*)malloc(sizeof(LilvNode));
		result->world = world;
		result->type = LILV_VALUE_BLANK;
		result->node = sord_node_copy(node);
		break;
	case SORD_LITERAL:
		datatype_uri = sord_node_get_datatype(node);
		if (datatype_uri) {
			if (sord_node_equals(datatype_uri, world->uris.xsd_boolean)) {
				type = LILV_VALUE_BOOL;
			}
			else if (sord_node_equals(datatype_uri, world->uris.xsd_decimal) ||
				sord_node_equals(datatype_uri, world->uris.xsd_double)) {
				type = LILV_VALUE_FLOAT;
			}
			else if (sord_node_equals(datatype_uri, world->uris.xsd_integer)) {
				type = LILV_VALUE_INT;
			}
			else if (sord_node_equals(datatype_uri,
				world->uris.xsd_base64Binary)) {
				type = LILV_VALUE_BLOB;
			}
			else {
				LILV_ERRORF("Unknown datatype `%s'\n",
					sord_node_get_string(datatype_uri));
			}
		}
		result = lilv_node_new(
			world, type, (const char*)sord_node_get_string(node));
		lilv_node_set_numerics_from_string(result);
		break;
	}

	return result;
}

LilvNode*
lilv_new_uri(LilvWorld* world, const char* uri)
{
	return lilv_node_new(world, LILV_VALUE_URI, uri);
}

LilvNode*
lilv_new_file_uri(LilvWorld* world, const char* host, const char* path)
{
	char* abs_path = lilv_path_absolute(path);
	SerdNode s = serd_node_new_file_uri(
		(const uint8_t*)abs_path, (const uint8_t*)host, NULL, true);

	LilvNode* ret = lilv_node_new(world, LILV_VALUE_URI, (const char*)s.buf);
	serd_node_free(&s);
	free(abs_path);
	return ret;
}

LilvNode*
lilv_new_string(LilvWorld* world, const char* str)
{
	return lilv_node_new(world, LILV_VALUE_STRING, str);
}

LilvNode*
lilv_new_int(LilvWorld* world, int val)
{
	char str[32];
	snprintf(str, sizeof(str), "%d", val);
	LilvNode* ret = lilv_node_new(world, LILV_VALUE_INT, str);
	ret->val.int_val = val;
	return ret;
}

LilvNode*
lilv_new_float(LilvWorld* world, float val)
{
	char str[32];
	snprintf(str, sizeof(str), "%f", val);
	LilvNode* ret = lilv_node_new(world, LILV_VALUE_FLOAT, str);
	ret->val.float_val = val;
	return ret;
}

LilvNode*
lilv_new_bool(LilvWorld* world, bool val)
{
	LilvNode* ret = lilv_node_new(world, LILV_VALUE_BOOL,
		val ? "true" : "false");
	ret->val.bool_val = val;
	return ret;
}

LilvNode*
lilv_node_duplicate(const LilvNode* val)
{
	if (!val) {
		return NULL;
	}

	LilvNode* result = (LilvNode*)malloc(sizeof(LilvNode));
	result->world = val->world;
	result->node = sord_node_copy(val->node);
	result->val = val->val;
	result->type = val->type;
	return result;
}

void
lilv_node_free(LilvNode* val)
{
	if (val) {
		sord_node_free(val->world->world, val->node);
		free(val);
	}
}

bool
lilv_node_equals(const LilvNode* value, const LilvNode* other)
{
	if (value == NULL && other == NULL) {
		return true;
	}
	else if (value == NULL || other == NULL) {
		return false;
	}
	else if (value->type != other->type) {
		return false;
	}

	switch (value->type) {
	case LILV_VALUE_URI:
	case LILV_VALUE_BLANK:
	case LILV_VALUE_STRING:
	case LILV_VALUE_BLOB:
		return sord_node_equals(value->node, other->node);
	case LILV_VALUE_INT:
		return (value->val.int_val == other->val.int_val);
	case LILV_VALUE_FLOAT:
		return (value->val.float_val == other->val.float_val);
	case LILV_VALUE_BOOL:
		return (value->val.bool_val == other->val.bool_val);
	}

	return false; /* shouldn't get here */
}

char*
lilv_node_get_turtle_token(const LilvNode* value)
{
	const char* str = (const char*)sord_node_get_string(value->node);
	size_t      len = 0;
	char* result = NULL;
	SerdNode    node;

	switch (value->type) {
	case LILV_VALUE_URI:
		len = strlen(str) + 3;
		result = (char*)calloc(len, 1);
		snprintf(result, len, "<%s>", str);
		break;
	case LILV_VALUE_BLANK:
		len = strlen(str) + 3;
		result = (char*)calloc(len, 1);
		snprintf(result, len, "_:%s", str);
		break;
	case LILV_VALUE_STRING:
	case LILV_VALUE_BOOL:
	case LILV_VALUE_BLOB:
		result = lilv_strdup(str);
		break;
	case LILV_VALUE_INT:
		node = serd_node_new_integer(value->val.int_val);
		result = lilv_strdup((char*)node.buf);
		serd_node_free(&node);
		break;
	case LILV_VALUE_FLOAT:
		node = serd_node_new_decimal(value->val.float_val, 8);
		result = lilv_strdup((char*)node.buf);
		serd_node_free(&node);
		break;
	}

	return result;
}

bool
lilv_node_is_uri(const LilvNode* value)
{
	return (value && value->type == LILV_VALUE_URI);
}

const char*
lilv_node_as_uri(const LilvNode* value)
{
	return (lilv_node_is_uri(value)
		? (const char*)sord_node_get_string(value->node)
		: NULL);
}

bool
lilv_node_is_blank(const LilvNode* value)
{
	return (value && value->type == LILV_VALUE_BLANK);
}

const char*
lilv_node_as_blank(const LilvNode* value)
{
	return (lilv_node_is_blank(value)
		? (const char*)sord_node_get_string(value->node)
		: NULL);
}

bool
lilv_node_is_literal(const LilvNode* value)
{
	if (!value) {
		return false;
	}

	switch (value->type) {
	case LILV_VALUE_STRING:
	case LILV_VALUE_INT:
	case LILV_VALUE_FLOAT:
	case LILV_VALUE_BLOB:
		return true;
	default:
		return false;
	}
}

bool
lilv_node_is_string(const LilvNode* value)
{
	return (value && value->type == LILV_VALUE_STRING);
}

const char*
lilv_node_as_string(const LilvNode* value)
{
	return value ? (const char*)sord_node_get_string(value->node) : NULL;
}

bool
lilv_node_is_int(const LilvNode* value)
{
	return (value && value->type == LILV_VALUE_INT);
}

int
lilv_node_as_int(const LilvNode* value)
{
	return lilv_node_is_int(value) ? value->val.int_val : 0;
}

bool
lilv_node_is_float(const LilvNode* value)
{
	return (value && value->type == LILV_VALUE_FLOAT);
}

float
lilv_node_as_float(const LilvNode* value)
{
	if (lilv_node_is_float(value)) {
		return value->val.float_val;
	}
	else if (lilv_node_is_int(value)) {
		return (float)value->val.int_val;
	}
	return NAN;
}

bool
lilv_node_is_bool(const LilvNode* value)
{
	return (value && value->type == LILV_VALUE_BOOL);
}

bool
lilv_node_as_bool(const LilvNode* value)
{
	return lilv_node_is_bool(value) ? value->val.bool_val : false;
}

char*
lilv_node_get_path(const LilvNode* value, char** hostname)
{
	if (lilv_node_is_uri(value)) {
		return lilv_file_uri_parse(lilv_node_as_uri(value), hostname);
	}
	return NULL;
}


#define NS_DOAP (const uint8_t*)"http://usefulinc.com/ns/doap#"
#define NS_FOAF (const uint8_t*)"http://xmlns.com/foaf/0.1/"

static void
lilv_plugin_init(LilvPlugin* plugin, LilvNode* bundle_uri)
{
	plugin->bundle_uri = bundle_uri;
	plugin->binary_uri = NULL;
#ifdef LILV_DYN_MANIFEST
	plugin->dynmanifest = NULL;
#endif
	plugin->plugin_class = NULL;
	plugin->data_uris = lilv_nodes_new();
	plugin->ports = NULL;
	plugin->num_ports = 0;
	plugin->loaded = false;
	plugin->parse_errors = false;
	plugin->replaced = false;
}

/** Ownership of `uri` and `bundle` is taken */
LilvPlugin*
lilv_plugin_new(LilvWorld* world, LilvNode* uri, LilvNode* bundle_uri)
{
	LilvPlugin* plugin = (LilvPlugin*)malloc(sizeof(LilvPlugin));

	plugin->world = world;
	plugin->plugin_uri = uri;

	lilv_plugin_init(plugin, bundle_uri);
	return plugin;
}

void
lilv_plugin_clear(LilvPlugin* plugin, LilvNode* bundle_uri)
{
	lilv_node_free(plugin->bundle_uri);
	lilv_node_free(plugin->binary_uri);
	lilv_nodes_free(plugin->data_uris);
	lilv_plugin_init(plugin, bundle_uri);
}

static void
lilv_plugin_free_ports(LilvPlugin* plugin)
{
	if (plugin->ports) {
		for (uint32_t i = 0; i < plugin->num_ports; ++i) {
			lilv_port_free(plugin, plugin->ports[i]);
		}
		free(plugin->ports);
		plugin->num_ports = 0;
		plugin->ports = NULL;
	}
}

void
lilv_plugin_free(LilvPlugin* plugin)
{
#ifdef LILV_DYN_MANIFEST
	if (plugin->dynmanifest && --plugin->dynmanifest->refs == 0) {
		lilv_dynmanifest_free(plugin->dynmanifest);
	}
#endif

	lilv_node_free(plugin->plugin_uri);
	plugin->plugin_uri = NULL;

	lilv_node_free(plugin->bundle_uri);
	plugin->bundle_uri = NULL;

	lilv_node_free(plugin->binary_uri);
	plugin->binary_uri = NULL;

	lilv_plugin_free_ports(plugin);

	lilv_nodes_free(plugin->data_uris);
	plugin->data_uris = NULL;

	free(plugin);
}

static LilvNode*
lilv_plugin_get_one(const LilvPlugin* plugin,
	const SordNode* subject,
	const SordNode* predicate)
{
	/* TODO: This is slower than it could be in some cases, but it's simpler to
	   use the existing i18n code. */

	SordIter* stream =
		lilv_world_query_internal(plugin->world, subject, predicate, NULL);

	LilvNodes* nodes = lilv_nodes_from_stream_objects(
		plugin->world, stream, SORD_OBJECT);

	if (nodes) {
		LilvNode* value = lilv_node_duplicate(lilv_nodes_get_first(nodes));
		lilv_nodes_free(nodes);
		return value;
	}

	return NULL;
}

LilvNode*
lilv_plugin_get_unique(const LilvPlugin* plugin,
	const SordNode* subject,
	const SordNode* predicate)
{
	LilvNode* ret = lilv_plugin_get_one(plugin, subject, predicate);
	if (!ret) {
		LILV_ERRORF("No value found for (%s %s ...) property\n",
			sord_node_get_string(subject),
			sord_node_get_string(predicate));
	}
	return ret;
}

static void
lilv_plugin_load(LilvPlugin* plugin)
{
	SordNode* bundle_uri_node = plugin->bundle_uri->node;
	const SerdNode* bundle_uri_snode = sord_node_to_serd_node(bundle_uri_node);

	SerdEnv* env = serd_env_new(bundle_uri_snode);
	SerdReader* reader = sord_new_reader(plugin->world->model, env, SERD_TURTLE,
		bundle_uri_node);

	SordModel* prots = lilv_world_filter_model(
		plugin->world,
		plugin->world->model,
		plugin->plugin_uri->node,
		plugin->world->uris.lv2_prototype,
		NULL, NULL);
	SordModel* skel = sord_new(plugin->world->world, SORD_SPO, false);
	SordIter* iter = sord_begin(prots);
	for (; !sord_iter_end(iter); sord_iter_next(iter)) {
		const SordNode* t = sord_iter_get_node(iter, SORD_OBJECT);
		LilvNode* prototype = lilv_node_new_from_node(plugin->world, t);

		lilv_world_load_resource(plugin->world, prototype);

		SordIter* statements = sord_search(
			plugin->world->model, prototype->node, NULL, NULL, NULL);
		FOREACH_MATCH(statements) {
			SordQuad quad;
			sord_iter_get(statements, quad);
			quad[0] = plugin->plugin_uri->node;
			sord_add(skel, quad);
		}

		sord_iter_free(statements);
		lilv_node_free(prototype);
	}
	sord_iter_free(iter);

	for (iter = sord_begin(skel); !sord_iter_end(iter); sord_iter_next(iter)) {
		SordQuad quad;
		sord_iter_get(iter, quad);
		sord_add(plugin->world->model, quad);
	}
	sord_iter_free(iter);
	sord_free(skel);
	sord_free(prots);

	// Parse all the plugin's data files into RDF model
	SerdStatus st = SERD_SUCCESS;
	LILV_FOREACH(nodes, i, plugin->data_uris) {
		const LilvNode* data_uri = lilv_nodes_get(plugin->data_uris, i);

		serd_env_set_base_uri(env, sord_node_to_serd_node(data_uri->node));
		st = lilv_world_load_file(plugin->world, reader, data_uri);
		if (st > SERD_FAILURE) {
			break;
		}
	}

	if (st > SERD_FAILURE) {
		plugin->loaded = true;
		plugin->parse_errors = true;
		serd_reader_free(reader);
		serd_env_free(env);
		return;
	}

#ifdef LILV_DYN_MANIFEST
	// Load and parse dynamic manifest data, if this is a library
	if (plugin->dynmanifest) {
		typedef int (*GetDataFunc)(LV2_Dyn_Manifest_Handle handle,
			FILE* fp,
			const char* uri);
		GetDataFunc get_data_func = (GetDataFunc)lilv_dlfunc(
			plugin->dynmanifest->lib, "lv2_dyn_manifest_get_data");
		if (get_data_func) {
			const SordNode* bundle = plugin->dynmanifest->bundle->node;
			serd_env_set_base_uri(env, sord_node_to_serd_node(bundle));
			FILE* fd = tmpfile();
			get_data_func(plugin->dynmanifest->handle, fd,
				lilv_node_as_string(plugin->plugin_uri));
			rewind(fd);
			serd_reader_add_blank_prefix(
				reader, lilv_world_blank_node_prefix(plugin->world));
			serd_reader_read_file_handle(
				reader, fd, (const uint8_t*)"(dyn-manifest)");
			fclose(fd);
		}
	}
#endif
	serd_reader_free(reader);
	serd_env_free(env);

	plugin->loaded = true;
}

static bool
is_symbol(const char* str)
{
	for (const char* s = str; *s; ++s) {
		if (!((*s >= 'a' && *s <= 'z') ||
			(*s >= 'A' && *s <= 'Z') ||
			(s > str && *s >= '0' && *s <= '9') ||
			*s == '_')) {
			return false;
		}
	}
	return true;
}

static void
lilv_plugin_load_ports_if_necessary(const LilvPlugin* const_plugin)
{
	LilvPlugin* plugin = (LilvPlugin*)const_plugin;

	lilv_plugin_load_if_necessary(plugin);

	if (!plugin->ports) {
		plugin->ports = (LilvPort**)malloc(sizeof(LilvPort*));
		plugin->ports[0] = NULL;

		SordIter* ports = lilv_world_query_internal(
			plugin->world,
			plugin->plugin_uri->node,
			plugin->world->uris.lv2_port,
			NULL);

		FOREACH_MATCH(ports) {
			const SordNode* port = sord_iter_get_node(ports, SORD_OBJECT);
			LilvNode* index = lilv_plugin_get_unique(
				plugin, port, plugin->world->uris.lv2_index);
			LilvNode* symbol = lilv_plugin_get_unique(
				plugin, port, plugin->world->uris.lv2_symbol);

			if (!lilv_node_is_string(symbol) ||
				!is_symbol((const char*)sord_node_get_string(symbol->node))) {
				LILV_ERRORF("Plugin <%s> port symbol `%s' is invalid\n",
					lilv_node_as_uri(plugin->plugin_uri),
					lilv_node_as_string(symbol));
				lilv_node_free(symbol);
				lilv_node_free(index);
				lilv_plugin_free_ports(plugin);
				break;
			}

			if (!lilv_node_is_int(index)) {
				LILV_ERRORF("Plugin <%s> port index is not an integer\n",
					lilv_node_as_uri(plugin->plugin_uri));
				lilv_node_free(symbol);
				lilv_node_free(index);
				lilv_plugin_free_ports(plugin);
				break;
			}

			uint32_t  this_index = lilv_node_as_int(index);
			LilvPort* this_port = NULL;
			if (plugin->num_ports > this_index) {
				this_port = plugin->ports[this_index];
			}
			else {
				plugin->ports = (LilvPort**)realloc(
					plugin->ports, (this_index + 1) * sizeof(LilvPort*));
				memset(plugin->ports + plugin->num_ports, '\0',
					(this_index - plugin->num_ports) * sizeof(LilvPort*));
				plugin->num_ports = this_index + 1;
			}

			// Havn't seen this port yet, add it to array
			if (!this_port) {
				this_port = lilv_port_new(plugin->world,
					port,
					this_index,
					lilv_node_as_string(symbol));
				plugin->ports[this_index] = this_port;
			}

			SordIter* types = lilv_world_query_internal(
				plugin->world, port, plugin->world->uris.rdf_a, NULL);
			FOREACH_MATCH(types) {
				const SordNode* type = sord_iter_get_node(types, SORD_OBJECT);
				if (sord_node_get_type(type) == SORD_URI) {
					zix_tree_insert(
						(ZixTree*)this_port->classes,
						lilv_node_new_from_node(plugin->world, type), NULL);
				}
				else {
					LILV_WARNF("Plugin <%s> port type is not a URI\n",
						lilv_node_as_uri(plugin->plugin_uri));
				}
			}
			sord_iter_free(types);

			lilv_node_free(symbol);
			lilv_node_free(index);
		}
		sord_iter_free(ports);

		// Check sanity
		for (uint32_t i = 0; i < plugin->num_ports; ++i) {
			if (!plugin->ports[i]) {
				LILV_ERRORF("Plugin <%s> is missing port %d/%d\n",
					lilv_node_as_uri(plugin->plugin_uri), i, plugin->num_ports);
				lilv_plugin_free_ports(plugin);
				break;
			}
		}
	}
}

void
lilv_plugin_load_if_necessary(const LilvPlugin* plugin)
{
	if (!plugin->loaded) {
		lilv_plugin_load((LilvPlugin*)plugin);
	}
}

const LilvNode*
lilv_plugin_get_uri(const LilvPlugin* plugin)
{
	return plugin->plugin_uri;
}

const LilvNode*
lilv_plugin_get_bundle_uri(const LilvPlugin* plugin)
{
	return plugin->bundle_uri;
}

const LilvNode*
lilv_plugin_get_library_uri(const LilvPlugin* plugin)
{
	lilv_plugin_load_if_necessary((LilvPlugin*)plugin);
	if (!plugin->binary_uri) {
		// <plugin> lv2:binary ?binary
		SordIter* i = lilv_world_query_internal(plugin->world,
			plugin->plugin_uri->node,
			plugin->world->uris.lv2_binary,
			NULL);
		FOREACH_MATCH(i) {
			const SordNode* binary_node = sord_iter_get_node(i, SORD_OBJECT);
			if (sord_node_get_type(binary_node) == SORD_URI) {
				((LilvPlugin*)plugin)->binary_uri =
					lilv_node_new_from_node(plugin->world, binary_node);
				break;
			}
		}
		sord_iter_free(i);
	}
	if (!plugin->binary_uri) {
		LILV_WARNF("Plugin <%s> has no lv2:binary\n",
			lilv_node_as_uri(lilv_plugin_get_uri(plugin)));
	}
	return plugin->binary_uri;
}

const LilvNodes*
lilv_plugin_get_data_uris(const LilvPlugin* plugin)
{
	return plugin->data_uris;
}

const LilvPluginClass*
lilv_plugin_get_class(const LilvPlugin* plugin)
{
	lilv_plugin_load_if_necessary((LilvPlugin*)plugin);
	if (!plugin->plugin_class) {
		// <plugin> a ?class
		SordIter* c = lilv_world_query_internal(plugin->world,
			plugin->plugin_uri->node,
			plugin->world->uris.rdf_a,
			NULL);
		FOREACH_MATCH(c) {
			const SordNode* class_node = sord_iter_get_node(c, SORD_OBJECT);
			if (sord_node_get_type(class_node) != SORD_URI) {
				continue;
			}

			LilvNode* klass = lilv_node_new_from_node(plugin->world, class_node);
			if (!lilv_node_equals(klass, plugin->world->lv2_plugin_class->uri)) {
				const LilvPluginClass* pclass = lilv_plugin_classes_get_by_uri(
					plugin->world->plugin_classes, klass);

				if (pclass) {
					((LilvPlugin*)plugin)->plugin_class = pclass;
					lilv_node_free(klass);
					break;
				}
			}

			lilv_node_free(klass);
		}
		sord_iter_free(c);

		if (plugin->plugin_class == NULL) {
			((LilvPlugin*)plugin)->plugin_class =
				plugin->world->lv2_plugin_class;
		}
	}
	return plugin->plugin_class;
}

static LilvNodes*
lilv_plugin_get_value_internal(const LilvPlugin* plugin,
	const SordNode* predicate)
{
	lilv_plugin_load_if_necessary(plugin);
	return lilv_world_find_nodes_internal(
		plugin->world, plugin->plugin_uri->node, predicate, NULL);
}

bool
lilv_plugin_verify(const LilvPlugin* plugin)
{
	lilv_plugin_load_if_necessary(plugin);
	if (plugin->parse_errors) {
		return false;
	}

	LilvNode* rdf_type = lilv_new_uri(plugin->world, LILV_NS_RDF "type");
	LilvNodes* results = lilv_plugin_get_value(plugin, rdf_type);
	lilv_node_free(rdf_type);
	if (!results) {
		return false;
	}

	lilv_nodes_free(results);
	results = lilv_plugin_get_value_internal(plugin,
		plugin->world->uris.doap_name);
	if (!results) {
		return false;
	}

	lilv_nodes_free(results);
	LilvNode* lv2_port = lilv_new_uri(plugin->world, LV2_CORE__port);
	results = lilv_plugin_get_value(plugin, lv2_port);
	lilv_node_free(lv2_port);
	if (!results) {
		return false;
	}

	lilv_nodes_free(results);
	return true;
}

LilvNode*
lilv_plugin_get_name(const LilvPlugin* plugin)
{
	LilvNodes* results = lilv_plugin_get_value_internal(
		plugin, plugin->world->uris.doap_name);

	LilvNode* ret = NULL;
	if (results) {
		LilvNode* val = lilv_nodes_get_first(results);
		if (lilv_node_is_string(val)) {
			ret = lilv_node_duplicate(val);
		}
		lilv_nodes_free(results);
	}

	if (!ret) {
		LILV_WARNF("Plugin <%s> has no (mandatory) doap:name\n",
			lilv_node_as_string(lilv_plugin_get_uri(plugin)));
	}

	return ret;
}

LilvNodes*
lilv_plugin_get_value(const LilvPlugin* plugin,
	const LilvNode* predicate)
{
	lilv_plugin_load_if_necessary(plugin);
	return lilv_world_find_nodes(plugin->world, plugin->plugin_uri, predicate, NULL);
}

uint32_t
lilv_plugin_get_num_ports(const LilvPlugin* plugin)
{
	lilv_plugin_load_ports_if_necessary(plugin);
	return plugin->num_ports;
}

void
lilv_plugin_get_port_ranges_float(const LilvPlugin* plugin,
	float* min_values,
	float* max_values,
	float* def_values)
{
	lilv_plugin_load_ports_if_necessary(plugin);
	LilvNode* min = NULL;
	LilvNode* max = NULL;
	LilvNode* def = NULL;
	LilvNode** minptr = min_values ? &min : NULL;
	LilvNode** maxptr = max_values ? &max : NULL;
	LilvNode** defptr = def_values ? &def : NULL;

	for (uint32_t i = 0; i < plugin->num_ports; ++i) {
		lilv_port_get_range(plugin, plugin->ports[i], defptr, minptr, maxptr);

		if (min_values) {
			if (lilv_node_is_float(min) || lilv_node_is_int(min)) {
				min_values[i] = lilv_node_as_float(min);
			}
			else {
				min_values[i] = NAN;
			}
		}

		if (max_values) {
			if (lilv_node_is_float(max) || lilv_node_is_int(max)) {
				max_values[i] = lilv_node_as_float(max);
			}
			else {
				max_values[i] = NAN;
			}
		}

		if (def_values) {
			if (lilv_node_is_float(def) || lilv_node_is_int(def)) {
				def_values[i] = lilv_node_as_float(def);
			}
			else {
				def_values[i] = NAN;
			}
		}

		lilv_node_free(def);
		lilv_node_free(min);
		lilv_node_free(max);
	}
}

uint32_t
lilv_plugin_get_num_ports_of_class_va(const LilvPlugin* plugin,
	const LilvNode* class_1,
	va_list           args)
{
	lilv_plugin_load_ports_if_necessary(plugin);

	uint32_t count = 0;

	// Build array of classes from args so we can walk it several times
	size_t           n_classes = 0;
	const LilvNode** classes = NULL;
	for (LilvNode* c = NULL; (c = va_arg(args, LilvNode*)); ) {
		classes = (const LilvNode**)realloc(
			classes, ++n_classes * sizeof(LilvNode*));
		classes[n_classes - 1] = c;
	}

	// Check each port against every type
	for (unsigned i = 0; i < plugin->num_ports; ++i) {
		LilvPort* port = plugin->ports[i];
		if (port && lilv_port_is_a(plugin, port, class_1)) {
			bool matches = true;
			for (size_t j = 0; j < n_classes; ++j) {
				if (!lilv_port_is_a(plugin, port, classes[j])) {
					matches = false;
					break;
				}
			}

			if (matches) {
				++count;
			}
		}
	}

	free(classes);
	return count;
}

uint32_t
lilv_plugin_get_num_ports_of_class(const LilvPlugin* plugin,
	const LilvNode* class_1, ...)
{
	va_list args;
	va_start(args, class_1);

	uint32_t count = lilv_plugin_get_num_ports_of_class_va(plugin, class_1, args);

	va_end(args);
	return count;
}

bool
lilv_plugin_has_latency(const LilvPlugin* plugin)
{
	lilv_plugin_load_if_necessary(plugin);
	SordIter* ports = lilv_world_query_internal(
		plugin->world,
		plugin->plugin_uri->node,
		plugin->world->uris.lv2_port,
		NULL);

	bool ret = false;
	FOREACH_MATCH(ports) {
		const SordNode* port = sord_iter_get_node(ports, SORD_OBJECT);
		SordIter* prop = lilv_world_query_internal(
			plugin->world,
			port,
			plugin->world->uris.lv2_portProperty,
			plugin->world->uris.lv2_reportsLatency);
		SordIter* des = lilv_world_query_internal(
			plugin->world,
			port,
			plugin->world->uris.lv2_designation,
			plugin->world->uris.lv2_latency);
		const bool latent = !sord_iter_end(prop) || !sord_iter_end(des);
		sord_iter_free(prop);
		sord_iter_free(des);
		if (latent) {
			ret = true;
			break;
		}
	}
	sord_iter_free(ports);

	return ret;
}

static const LilvPort*
lilv_plugin_get_port_by_property(const LilvPlugin* plugin,
	const SordNode* port_property)
{
	lilv_plugin_load_ports_if_necessary(plugin);
	for (uint32_t i = 0; i < plugin->num_ports; ++i) {
		LilvPort* port = plugin->ports[i];
		SordIter* iter = lilv_world_query_internal(
			plugin->world,
			port->node->node,
			plugin->world->uris.lv2_portProperty,
			port_property);

		const bool found = !sord_iter_end(iter);
		sord_iter_free(iter);

		if (found) {
			return port;
		}
	}

	return NULL;
}

const LilvPort*
lilv_plugin_get_port_by_designation(const LilvPlugin* plugin,
	const LilvNode* port_class,
	const LilvNode* designation)
{
	LilvWorld* world = plugin->world;
	lilv_plugin_load_ports_if_necessary(plugin);
	for (uint32_t i = 0; i < plugin->num_ports; ++i) {
		LilvPort* port = plugin->ports[i];
		SordIter* iter = lilv_world_query_internal(
			world,
			port->node->node,
			world->uris.lv2_designation,
			designation->node);

		const bool found = !sord_iter_end(iter) &&
			(!port_class || lilv_port_is_a(plugin, port, port_class));
		sord_iter_free(iter);

		if (found) {
			return port;
		}
	}

	return NULL;
}

uint32_t
lilv_plugin_get_latency_port_index(const LilvPlugin* plugin)
{
	LilvNode* lv2_OutputPort =
		lilv_new_uri(plugin->world, LV2_CORE__OutputPort);
	LilvNode* lv2_latency =
		lilv_new_uri(plugin->world, LV2_CORE__latency);

	const LilvPort* prop_port = lilv_plugin_get_port_by_property(
		plugin, plugin->world->uris.lv2_reportsLatency);
	const LilvPort* des_port = lilv_plugin_get_port_by_designation(
		plugin, lv2_OutputPort, lv2_latency);

	lilv_node_free(lv2_latency);
	lilv_node_free(lv2_OutputPort);

	if (prop_port) {
		return prop_port->index;
	}
	else if (des_port) {
		return des_port->index;
	}
	else {
		return (uint32_t)-1;
	}
}

bool
lilv_plugin_has_feature(const LilvPlugin* plugin,
	const LilvNode* feature)
{
	lilv_plugin_load_if_necessary(plugin);
	const SordNode* predicates[] = { plugin->world->uris.lv2_requiredFeature,
									 plugin->world->uris.lv2_optionalFeature,
									 NULL };

	for (const SordNode** pred = predicates; *pred; ++pred) {
		if (lilv_world_ask_internal(
			plugin->world, plugin->plugin_uri->node, *pred, feature->node)) {
			return true;
		}
	}
	return false;
}

LilvNodes*
lilv_plugin_get_supported_features(const LilvPlugin* plugin)
{
	LilvNodes* optional = lilv_plugin_get_optional_features(plugin);
	LilvNodes* required = lilv_plugin_get_required_features(plugin);
	LilvNodes* result = lilv_nodes_merge(optional, required);
	lilv_nodes_free(optional);
	lilv_nodes_free(required);
	return result;
}

LilvNodes*
lilv_plugin_get_optional_features(const LilvPlugin* plugin)
{
	lilv_plugin_load_if_necessary(plugin);
	return lilv_world_find_nodes_internal(plugin->world,
		plugin->plugin_uri->node,
		plugin->world->uris.lv2_optionalFeature,
		NULL);
}

LilvNodes*
lilv_plugin_get_required_features(const LilvPlugin* plugin)
{
	lilv_plugin_load_if_necessary(plugin);
	return lilv_world_find_nodes_internal(plugin->world,
		plugin->plugin_uri->node,
		plugin->world->uris.lv2_requiredFeature,
		NULL);
}

bool
lilv_plugin_has_extension_data(const LilvPlugin* plugin,
	const LilvNode* uri)
{
	if (!lilv_node_is_uri(uri)) {
		LILV_ERRORF("Extension data `%s' is not a URI\n",
			sord_node_get_string(uri->node));
		return false;
	}

	lilv_plugin_load_if_necessary(plugin);
	return lilv_world_ask_internal(
		plugin->world,
		plugin->plugin_uri->node,
		plugin->world->uris.lv2_extensionData,
		uri->node);
}

LilvNodes*
lilv_plugin_get_extension_data(const LilvPlugin* plugin)
{
	return lilv_plugin_get_value_internal(plugin, plugin->world->uris.lv2_extensionData);
}

const LilvPort*
lilv_plugin_get_port_by_index(const LilvPlugin* plugin,
	uint32_t          index)
{
	lilv_plugin_load_ports_if_necessary(plugin);
	if (index < plugin->num_ports) {
		return plugin->ports[index];
	}
	else {
		return NULL;
	}
}

const LilvPort*
lilv_plugin_get_port_by_symbol(const LilvPlugin* plugin,
	const LilvNode* symbol)
{
	lilv_plugin_load_ports_if_necessary(plugin);
	for (uint32_t i = 0; i < plugin->num_ports; ++i) {
		LilvPort* port = plugin->ports[i];
		if (lilv_node_equals(port->symbol, symbol)) {
			return port;
		}
	}

	return NULL;
}

LilvNode*
lilv_plugin_get_project(const LilvPlugin* plugin)
{
	lilv_plugin_load_if_necessary(plugin);

	SordNode* lv2_project = sord_new_uri(plugin->world->world,
		(const uint8_t*)LV2_CORE__project);

	SordIter* projects = lilv_world_query_internal(plugin->world,
		plugin->plugin_uri->node,
		lv2_project,
		NULL);

	sord_node_free(plugin->world->world, lv2_project);

	if (sord_iter_end(projects)) {
		sord_iter_free(projects);
		return NULL;
	}

	const SordNode* project = sord_iter_get_node(projects, SORD_OBJECT);

	sord_iter_free(projects);
	return lilv_node_new_from_node(plugin->world, project);
}

static const SordNode*
lilv_plugin_get_author(const LilvPlugin* plugin)
{
	lilv_plugin_load_if_necessary(plugin);

	SordNode* doap_maintainer = sord_new_uri(
		plugin->world->world, NS_DOAP "maintainer");

	SordIter* maintainers = lilv_world_query_internal(
		plugin->world,
		plugin->plugin_uri->node,
		doap_maintainer,
		NULL);

	if (sord_iter_end(maintainers)) {
		sord_iter_free(maintainers);

		LilvNode* project = lilv_plugin_get_project(plugin);
		if (!project) {
			sord_node_free(plugin->world->world, doap_maintainer);
			return NULL;
		}

		maintainers = lilv_world_query_internal(
			plugin->world,
			project->node,
			doap_maintainer,
			NULL);

		lilv_node_free(project);
	}

	sord_node_free(plugin->world->world, doap_maintainer);

	if (sord_iter_end(maintainers)) {
		sord_iter_free(maintainers);
		return NULL;
	}

	const SordNode* author = sord_iter_get_node(maintainers, SORD_OBJECT);

	sord_iter_free(maintainers);
	return author;
}

static LilvNode*
lilv_plugin_get_author_property(const LilvPlugin* plugin, const uint8_t* uri)
{
	const SordNode* author = lilv_plugin_get_author(plugin);
	if (author) {
		SordWorld* sworld = plugin->world->world;
		SordNode* pred = sord_new_uri(sworld, uri);
		LilvNode* ret = lilv_plugin_get_one(plugin, author, pred);
		sord_node_free(sworld, pred);
		return ret;
	}
	return NULL;
}

LilvNode*
lilv_plugin_get_author_name(const LilvPlugin* plugin)
{
	return lilv_plugin_get_author_property(plugin, NS_FOAF "name");
}

LilvNode*
lilv_plugin_get_author_email(const LilvPlugin* plugin)
{
	return lilv_plugin_get_author_property(plugin, NS_FOAF "mbox");
}

LilvNode*
lilv_plugin_get_author_homepage(const LilvPlugin* plugin)
{
	return lilv_plugin_get_author_property(plugin, NS_FOAF "homepage");
}

bool
lilv_plugin_is_replaced(const LilvPlugin* plugin)
{
	return plugin->replaced;
}

LilvUIs*
lilv_plugin_get_uis(const LilvPlugin* plugin)
{
	lilv_plugin_load_if_necessary(plugin);

	SordNode* ui_ui_node = sord_new_uri(plugin->world->world,
		(const uint8_t*)LV2_UI__ui);
	SordNode* ui_binary_node = sord_new_uri(plugin->world->world,
		(const uint8_t*)LV2_UI__binary);

	LilvUIs* result = lilv_uis_new();
	SordIter* uis = lilv_world_query_internal(plugin->world,
		plugin->plugin_uri->node,
		ui_ui_node,
		NULL);

	FOREACH_MATCH(uis) {
		const SordNode* ui = sord_iter_get_node(uis, SORD_OBJECT);

		LilvNode* type = lilv_plugin_get_unique(plugin, ui, plugin->world->uris.rdf_a);
		LilvNode* binary = lilv_plugin_get_one(plugin, ui, plugin->world->uris.lv2_binary);
		if (!binary) {
			binary = lilv_plugin_get_unique(plugin, ui, ui_binary_node);
		}

		if (sord_node_get_type(ui) != SORD_URI
			|| !lilv_node_is_uri(type)
			|| !lilv_node_is_uri(binary)) {
			lilv_node_free(binary);
			lilv_node_free(type);
			LILV_ERRORF("Corrupt UI <%s>\n", sord_node_get_string(ui));
			continue;
		}

		LilvUI* lilv_ui = lilv_ui_new(
			plugin->world,
			lilv_node_new_from_node(plugin->world, ui),
			type,
			binary);

		zix_tree_insert((ZixTree*)result, lilv_ui, NULL);
	}
	sord_iter_free(uis);

	sord_node_free(plugin->world->world, ui_binary_node);
	sord_node_free(plugin->world->world, ui_ui_node);

	if (lilv_uis_size(result) > 0) {
		return result;
	}
	else {
		lilv_uis_free(result);
		return NULL;
	}
}

LilvNodes*
lilv_plugin_get_related(const LilvPlugin* plugin, const LilvNode* type)
{
	lilv_plugin_load_if_necessary(plugin);

	LilvWorld* const world = plugin->world;
	LilvNodes* const related = lilv_world_find_nodes_internal(
		world,
		NULL,
		world->uris.lv2_appliesTo,
		lilv_plugin_get_uri(plugin)->node);

	if (!type) {
		return related;
	}

	LilvNodes* matches = lilv_nodes_new();
	LILV_FOREACH(nodes, i, related) {
		LilvNode* node = (LilvNode*)lilv_collection_get((ZixTree*)related, i);
		if (lilv_world_ask_internal(
			world, node->node, world->uris.rdf_a, type->node)) {
			zix_tree_insert((ZixTree*)matches,
				lilv_node_new_from_node(world, node->node),
				NULL);
		}
	}

	lilv_nodes_free(related);
	return matches;
}

static SerdEnv*
new_lv2_env(const SerdNode* base)
{
	SerdEnv* env = serd_env_new(base);

#define USTR(s) ((const uint8_t*)(s))
	serd_env_set_prefix_from_strings(env, USTR("doap"), USTR(LILV_NS_DOAP));
	serd_env_set_prefix_from_strings(env, USTR("foaf"), USTR(LILV_NS_FOAF));
	serd_env_set_prefix_from_strings(env, USTR("lv2"), USTR(LILV_NS_LV2));
	serd_env_set_prefix_from_strings(env, USTR("owl"), USTR(LILV_NS_OWL));
	serd_env_set_prefix_from_strings(env, USTR("rdf"), USTR(LILV_NS_RDF));
	serd_env_set_prefix_from_strings(env, USTR("rdfs"), USTR(LILV_NS_RDFS));
	serd_env_set_prefix_from_strings(env, USTR("xsd"), USTR(LILV_NS_XSD));

	return env;
}

static void
maybe_write_prefixes(SerdWriter* writer, SerdEnv* env, FILE* file)
{
	fseek(file, 0, SEEK_END);
	if (ftell(file) == 0) {
		serd_env_foreach(
			env, (SerdPrefixSink)serd_writer_set_prefix, writer);
	}
	else {
		fprintf(file, "\n");
	}
}

void
lilv_plugin_write_description(LilvWorld* world,
	const LilvPlugin* plugin,
	const LilvNode* base_uri,
	FILE* plugin_file)
{
	const LilvNode* subject = lilv_plugin_get_uri(plugin);
	const uint32_t  num_ports = lilv_plugin_get_num_ports(plugin);
	const SerdNode* base = sord_node_to_serd_node(base_uri->node);
	SerdEnv* env = new_lv2_env(base);

	SerdWriter* writer = serd_writer_new(
		SERD_TURTLE,
		(SerdStyle)(SERD_STYLE_ABBREVIATED | SERD_STYLE_CURIED),
		env,
		NULL,
		serd_file_sink,
		plugin_file);

	// Write prefixes if this is a new file
	maybe_write_prefixes(writer, env, plugin_file);

	// Write plugin description
	SordIter* plug_iter = lilv_world_query_internal(
		world, subject->node, NULL, NULL);
	sord_write_iter(plug_iter, writer);

	// Write port descriptions
	for (uint32_t i = 0; i < num_ports; ++i) {
		const LilvPort* port = plugin->ports[i];
		SordIter* port_iter = lilv_world_query_internal(
			world, port->node->node, NULL, NULL);
		sord_write_iter(port_iter, writer);
	}

	serd_writer_free(writer);
	serd_env_free(env);
}

void
lilv_plugin_write_manifest_entry(LilvWorld* world,
	const LilvPlugin* plugin,
	const LilvNode* base_uri,
	FILE* manifest_file,
	const char* plugin_file_path)
{
	const LilvNode* subject = lilv_plugin_get_uri(plugin);
	const SerdNode* base = sord_node_to_serd_node(base_uri->node);
	SerdEnv* env = new_lv2_env(base);

	SerdWriter* writer = serd_writer_new(
		SERD_TURTLE,
		(SerdStyle)(SERD_STYLE_ABBREVIATED | SERD_STYLE_CURIED),
		env,
		NULL,
		serd_file_sink,
		manifest_file);

	// Write prefixes if this is a new file
	maybe_write_prefixes(writer, env, manifest_file);

	// Write manifest entry
	serd_writer_write_statement(
		writer, 0, NULL,
		sord_node_to_serd_node(subject->node),
		sord_node_to_serd_node(plugin->world->uris.rdf_a),
		sord_node_to_serd_node(plugin->world->uris.lv2_Plugin), 0, 0);

	const SerdNode file_node = serd_node_from_string(
		SERD_URI, (const uint8_t*)plugin_file_path);
	serd_writer_write_statement(
		writer, 0, NULL,
		sord_node_to_serd_node(subject->node),
		sord_node_to_serd_node(plugin->world->uris.rdfs_seeAlso),
		&file_node, 0, 0);

	serd_writer_free(writer);
	serd_env_free(env);
}


LilvPluginClass*
lilv_plugin_class_new(LilvWorld* world,
	const SordNode* parent_node,
	const SordNode* uri,
	const char* label)
{
	LilvPluginClass* pc = (LilvPluginClass*)malloc(sizeof(LilvPluginClass));
	pc->world = world;
	pc->uri = lilv_node_new_from_node(world, uri);
	pc->label = lilv_node_new(world, LILV_VALUE_STRING, label);
	pc->parent_uri = (parent_node
		? lilv_node_new_from_node(world, parent_node)
		: NULL);
	return pc;
}

void
lilv_plugin_class_free(LilvPluginClass* plugin_class)
{
	if (!plugin_class) {
		return;
	}

	lilv_node_free(plugin_class->uri);
	lilv_node_free(plugin_class->parent_uri);
	lilv_node_free(plugin_class->label);
	free(plugin_class);
}

const LilvNode*
lilv_plugin_class_get_parent_uri(const LilvPluginClass* plugin_class)
{
	return plugin_class->parent_uri ? plugin_class->parent_uri : NULL;
}

const LilvNode*
lilv_plugin_class_get_uri(const LilvPluginClass* plugin_class)
{
	return plugin_class->uri;
}

const LilvNode*
lilv_plugin_class_get_label(const LilvPluginClass* plugin_class)
{
	return plugin_class->label;
}

LilvPluginClasses*
lilv_plugin_class_get_children(const LilvPluginClass* plugin_class)
{
	// Returned list doesn't own categories
	LilvPluginClasses* all = plugin_class->world->plugin_classes;
	LilvPluginClasses* result = zix_tree_new(false, lilv_ptr_cmp, NULL, NULL);

	for (ZixTreeIter* i = zix_tree_begin((ZixTree*)all);
		i != zix_tree_end((ZixTree*)all);
		i = zix_tree_iter_next(i)) {
		const LilvPluginClass* c = (LilvPluginClass*)zix_tree_get(i);
		const LilvNode* parent = lilv_plugin_class_get_parent_uri(c);
		if (parent && lilv_node_equals(lilv_plugin_class_get_uri(plugin_class),
			parent)) {
			zix_tree_insert((ZixTree*)result, (LilvPluginClass*)c, NULL);
		}
	}

	return result;
}


LilvPort*
lilv_port_new(LilvWorld* world,
	const SordNode* node,
	uint32_t        index,
	const char* symbol)
{
	LilvPort* port = (LilvPort*)malloc(sizeof(LilvPort));
	port->node = lilv_node_new_from_node(world, node);
	port->index = index;
	port->symbol = lilv_node_new(world, LILV_VALUE_STRING, symbol);
	port->classes = lilv_nodes_new();
	return port;
}

void
lilv_port_free(const LilvPlugin* plugin, LilvPort* port)
{
	if (port) {
		lilv_node_free(port->node);
		lilv_nodes_free(port->classes);
		lilv_node_free(port->symbol);
		free(port);
	}
}

bool
lilv_port_is_a(const LilvPlugin* plugin,
	const LilvPort* port,
	const LilvNode* port_class)
{
	LILV_FOREACH(nodes, i, port->classes) {
		if (lilv_node_equals(lilv_nodes_get(port->classes, i), port_class)) {
			return true;
		}
	}

	return false;
}

bool
lilv_port_has_property(const LilvPlugin* plugin,
	const LilvPort* port,
	const LilvNode* property)
{
	return lilv_world_ask_internal(plugin->world,
		port->node->node,
		plugin->world->uris.lv2_portProperty,
		property->node);
}

bool
lilv_port_supports_event(const LilvPlugin* plugin,
	const LilvPort* port,
	const LilvNode* event_type)
{
	const uint8_t* predicates[] = { (const uint8_t*)LV2_EVENT__supportsEvent,
									(const uint8_t*)LV2_ATOM__supports,
									NULL };

	for (const uint8_t** pred = predicates; *pred; ++pred) {
		if (lilv_world_ask_internal(plugin->world,
			port->node->node,
			sord_new_uri(plugin->world->world, *pred),
			event_type->node)) {
			return true;
		}
	}
	return false;
}

static LilvNodes*
lilv_port_get_value_by_node(const LilvPlugin* plugin,
	const LilvPort* port,
	const SordNode* predicate)
{
	return lilv_world_find_nodes_internal(plugin->world,
		port->node->node,
		predicate,
		NULL);
}

const LilvNode*
lilv_port_get_node(const LilvPlugin* plugin,
	const LilvPort* port)
{
	return port->node;
}

LilvNodes*
lilv_port_get_value(const LilvPlugin* plugin,
	const LilvPort* port,
	const LilvNode* predicate)
{
	if (!lilv_node_is_uri(predicate)) {
		LILV_ERRORF("Predicate `%s' is not a URI\n",
			sord_node_get_string(predicate->node));
		return NULL;
	}

	return lilv_port_get_value_by_node(plugin, port, predicate->node);
}

LilvNode*
lilv_port_get(const LilvPlugin* plugin,
	const LilvPort* port,
	const LilvNode* predicate)
{
	LilvNodes* values = lilv_port_get_value(plugin, port, predicate);

	LilvNode* value = lilv_node_duplicate(
		values ? lilv_nodes_get_first(values) : NULL);

	lilv_nodes_free(values);
	return value;
}

uint32_t
lilv_port_get_index(const LilvPlugin* plugin,
	const LilvPort* port)
{
	return port->index;
}

const LilvNode*
lilv_port_get_symbol(const LilvPlugin* plugin,
	const LilvPort* port)
{
	return port->symbol;
}

LilvNode*
lilv_port_get_name(const LilvPlugin* plugin,
	const LilvPort* port)
{
	LilvNodes* results = lilv_port_get_value_by_node(
		plugin, port, plugin->world->uris.lv2_name);

	LilvNode* ret = NULL;
	if (results) {
		LilvNode* val = lilv_nodes_get_first(results);
		if (lilv_node_is_string(val)) {
			ret = lilv_node_duplicate(val);
		}
		lilv_nodes_free(results);
	}

	if (!ret) {
		LILV_WARNF("Plugin <%s> port has no (mandatory) doap:name\n",
			lilv_node_as_string(lilv_plugin_get_uri(plugin)));
	}

	return ret;
}

const LilvNodes*
lilv_port_get_classes(const LilvPlugin* plugin,
	const LilvPort* port)
{
	return port->classes;
}

void
lilv_port_get_range(const LilvPlugin* plugin,
	const LilvPort* port,
	LilvNode** def,
	LilvNode** min,
	LilvNode** max)
{
	if (def) {
		LilvNodes* defaults = lilv_port_get_value_by_node(
			plugin, port, plugin->world->uris.lv2_default);
		*def = defaults
			? lilv_node_duplicate(lilv_nodes_get_first(defaults))
			: NULL;
		lilv_nodes_free(defaults);
	}
	if (min) {
		LilvNodes* minimums = lilv_port_get_value_by_node(
			plugin, port, plugin->world->uris.lv2_minimum);
		*min = minimums
			? lilv_node_duplicate(lilv_nodes_get_first(minimums))
			: NULL;
		lilv_nodes_free(minimums);
	}
	if (max) {
		LilvNodes* maximums = lilv_port_get_value_by_node(
			plugin, port, plugin->world->uris.lv2_maximum);
		*max = maximums
			? lilv_node_duplicate(lilv_nodes_get_first(maximums))
			: NULL;
		lilv_nodes_free(maximums);
	}
}

LilvScalePoints*
lilv_port_get_scale_points(const LilvPlugin* plugin,
	const LilvPort* port)
{
	SordIter* points = lilv_world_query_internal(
		plugin->world,
		port->node->node,
		sord_new_uri(plugin->world->world, (const uint8_t*)LV2_CORE__scalePoint),
		NULL);

	LilvScalePoints* ret = NULL;
	if (!sord_iter_end(points)) {
		ret = lilv_scale_points_new();
	}

	FOREACH_MATCH(points) {
		const SordNode* point = sord_iter_get_node(points, SORD_OBJECT);

		LilvNode* value = lilv_plugin_get_unique(plugin,
			point,
			plugin->world->uris.rdf_value);

		LilvNode* label = lilv_plugin_get_unique(plugin,
			point,
			plugin->world->uris.rdfs_label);

		if (value && label) {
			zix_tree_insert(
				(ZixTree*)ret, lilv_scale_point_new(value, label), NULL);
		}
	}
	sord_iter_free(points);

	assert(!ret || lilv_nodes_size(ret) > 0);
	return ret;
}

LilvNodes*
lilv_port_get_properties(const LilvPlugin* plugin,
	const LilvPort* port)
{
	LilvNode* pred = lilv_node_new_from_node(
		plugin->world, plugin->world->uris.lv2_portProperty);
	LilvNodes* ret = lilv_port_get_value(plugin, port, pred);
	lilv_node_free(pred);
	return ret;
}


typedef enum {
	LILV_LANG_MATCH_NONE,     ///< Language does not match at all
	LILV_LANG_MATCH_PARTIAL,  ///< Partial (language, but not country) match
	LILV_LANG_MATCH_EXACT     ///< Exact (language and country) match
} LilvLangMatch;

static LilvLangMatch
lilv_lang_matches(const char* a, const char* b)
{
	if (!a || !b) {
		return LILV_LANG_MATCH_NONE;
	}
	else if (!strcmp(a, b)) {
		return LILV_LANG_MATCH_EXACT;
	}

	const char* a_dash = strchr(a, '-');
	const size_t a_lang_len = a_dash ? (size_t)(a_dash - a) : strlen(a);
	const char* b_dash = strchr(b, '-');
	const size_t b_lang_len = b_dash ? (size_t)(b_dash - b) : strlen(b);

	if (a_lang_len == b_lang_len && !strncmp(a, b, a_lang_len)) {
		return LILV_LANG_MATCH_PARTIAL;
	}

	return LILV_LANG_MATCH_NONE;
}

static LilvNodes*
lilv_nodes_from_stream_objects_i18n(LilvWorld* world,
	SordIter* stream,
	SordQuadIndex field)
{
	LilvNodes* values = lilv_nodes_new();
	const SordNode* nolang = NULL;  // Untranslated value
	const SordNode* partial = NULL;  // Partial language match
	char* syslang = lilv_get_lang();
	FOREACH_MATCH(stream) {
		const SordNode* value = sord_iter_get_node(stream, field);
		if (sord_node_get_type(value) == SORD_LITERAL) {
			const char* lang = sord_node_get_language(value);

			if (!lang) {
				nolang = value;
			}
			else {
				switch (lilv_lang_matches(lang, syslang)) {
				case LILV_LANG_MATCH_EXACT:
					// Exact language match, add to results
					zix_tree_insert((ZixTree*)values,
						lilv_node_new_from_node(world, value),
						NULL);
					break;
				case LILV_LANG_MATCH_PARTIAL:
					// Partial language match, save in case we find no exact
					partial = value;
					break;
				case LILV_LANG_MATCH_NONE:
					break;
				}
			}
		}
		else {
			zix_tree_insert((ZixTree*)values,
				lilv_node_new_from_node(world, value),
				NULL);
		}
	}
	sord_iter_free(stream);
	free(syslang);

	if (lilv_nodes_size(values) > 0) {
		return values;
	}

	const SordNode* best = nolang;
	if (syslang && partial) {
		// Partial language match for system language
		best = partial;
	}
	else if (!best) {
		// No languages matches at all, and no untranslated value
		// Use any value, if possible
		best = partial;
	}

	if (best) {
		zix_tree_insert(
			(ZixTree*)values, lilv_node_new_from_node(world, best), NULL);
	}
	else {
		// No matches whatsoever
		lilv_nodes_free(values);
		values = NULL;
	}

	return values;
}

LilvNodes*
lilv_nodes_from_stream_objects(LilvWorld* world,
	SordIter* stream,
	SordQuadIndex field)
{
	if (sord_iter_end(stream)) {
		sord_iter_free(stream);
		return NULL;
	}
	else if (world->opt.filter_language) {
		return lilv_nodes_from_stream_objects_i18n(world, stream, field);
	}
	else {
		LilvNodes* values = lilv_nodes_new();
		FOREACH_MATCH(stream) {
			const SordNode* value = sord_iter_get_node(stream, field);
			LilvNode* node = lilv_node_new_from_node(world, value);
			if (node) {
				zix_tree_insert((ZixTree*)values, node, NULL);
			}
		}
		sord_iter_free(stream);
		return values;
	}
}


/** Ownership of value and label is taken */
LilvScalePoint*
lilv_scale_point_new(LilvNode* value, LilvNode* label)
{
	LilvScalePoint* point = (LilvScalePoint*)malloc(sizeof(LilvScalePoint));
	point->value = value;
	point->label = label;
	return point;
}

void
lilv_scale_point_free(LilvScalePoint* point)
{
	if (point) {
		lilv_node_free(point->value);
		lilv_node_free(point->label);
		free(point);
	}
}

const LilvNode*
lilv_scale_point_get_value(const LilvScalePoint* point)
{
	return point->value;
}

const LilvNode*
lilv_scale_point_get_label(const LilvScalePoint* point)
{
	return point->label;
}


#define USTR(s) ((const uint8_t*)(s))

typedef struct {
	void* value;  ///< Value/Object
	size_t   size;   ///< Size of value
	uint32_t key;    ///< Key/Predicate (URID)
	uint32_t type;   ///< Type of value (URID)
	uint32_t flags;  ///< State flags (POD, etc)
} Property;

typedef struct {
	char* symbol; ///< Symbol of port
	LV2_Atom* atom;   ///< Value in port
} PortValue;

typedef struct {
	char* abs;  ///< Absolute path of actual file
	char* rel;  ///< Abstract path (relative path in state dir)
} PathMap;

typedef struct {
	size_t    n;
	Property* props;
} PropertyArray;

struct LilvStateImpl {
	LilvNode* plugin_uri;   ///< Plugin URI
	LilvNode* uri;          ///< State/preset URI
	char* dir;          ///< Save directory (if saved)
	char* scratch_dir;  ///< Directory for files created by plugin
	char* copy_dir;     ///< Directory for snapshots of external files
	char* link_dir;     ///< Directory for links to external files
	char* label;        ///< State/Preset label
	ZixTree* abs2rel;      ///< PathMap sorted by abs
	ZixTree* rel2abs;      ///< PathMap sorted by rel
	PropertyArray props;        ///< State properties
	PropertyArray metadata;     ///< State metadata
	PortValue* values;       ///< Port values
	uint32_t      atom_Path;    ///< atom:Path URID
	uint32_t      n_values;     ///< Number of port values
};

static int
abs_cmp(const void* a, const void* b, void* user_data)
{
	return strcmp(((const PathMap*)a)->abs, ((const PathMap*)b)->abs);
}

static int
rel_cmp(const void* a, const void* b, void* user_data)
{
	return strcmp(((const PathMap*)a)->rel, ((const PathMap*)b)->rel);
}

static int
property_cmp(const void* a, const void* b)
{
	return ((const Property*)a)->key - ((const Property*)b)->key;
}

static int
value_cmp(const void* a, const void* b)
{
	return strcmp(((const PortValue*)a)->symbol,
		((const PortValue*)b)->symbol);
}

static void
path_rel_free(void* ptr)
{
	free(((PathMap*)ptr)->abs);
	free(((PathMap*)ptr)->rel);
	free(ptr);
}

static PortValue*
append_port_value(LilvState* state,
	const char* port_symbol,
	const void* value,
	uint32_t    size,
	uint32_t    type)
{
	PortValue* pv = NULL;
	if (value) {
		state->values = (PortValue*)realloc(
			state->values, (++state->n_values) * sizeof(PortValue));

		pv = &state->values[state->n_values - 1];
		pv->symbol = lilv_strdup(port_symbol);
		pv->atom = (LV2_Atom*)malloc(sizeof(LV2_Atom) + size);
		pv->atom->size = size;
		pv->atom->type = type;
		memcpy(pv->atom + 1, value, size);
	}
	return pv;
}

static const char*
lilv_state_rel2abs(const LilvState* state, const char* path)
{
	ZixTreeIter* iter = NULL;
	const PathMap key = { NULL, (char*)path };
	if (state->rel2abs && !zix_tree_find(state->rel2abs, &key, &iter)) {
		return ((const PathMap*)zix_tree_get(iter))->abs;
	}
	return path;
}

static void
append_property(LilvState* state,
	PropertyArray* array,
	uint32_t       key,
	const void* value,
	size_t         size,
	uint32_t       type,
	uint32_t       flags)
{
	array->props = (Property*)realloc(
		array->props, (++array->n) * sizeof(Property));

	Property* const prop = &array->props[array->n - 1];
	if ((flags & LV2_STATE_IS_POD) || type == state->atom_Path) {
		prop->value = malloc(size);
		memcpy(prop->value, value, size);
	}
	else {
		prop->value = (void*)value;
	}

	prop->size = size;
	prop->key = key;
	prop->type = type;
	prop->flags = flags;
}

static const Property*
find_property(const LilvState* const state, const uint32_t key)
{
	const Property search_key = { NULL, 0, key, 0, 0 };

	return (const Property*)bsearch(&search_key,
		state->props.props,
		state->props.n,
		sizeof(Property),
		property_cmp);
}

static LV2_State_Status
store_callback(LV2_State_Handle handle,
	uint32_t         key,
	const void* value,
	size_t           size,
	uint32_t         type,
	uint32_t         flags)
{
	LilvState* const state = (LilvState*)handle;

	if (!key) {
		return LV2_STATE_ERR_UNKNOWN; // TODO: Add status for bad arguments
	}

	if (find_property((const LilvState*)handle, key)) {
		return LV2_STATE_ERR_UNKNOWN; // TODO: Add status for duplicate keys
	}

	append_property(state, &state->props, key, value, size, type, flags);
	return LV2_STATE_SUCCESS;
}

static const void*
retrieve_callback(LV2_State_Handle handle,
	uint32_t         key,
	size_t* size,
	uint32_t* type,
	uint32_t* flags)
{
	const Property* const prop = find_property((const LilvState*)handle, key);

	if (prop) {
		*size = prop->size;
		*type = prop->type;
		*flags = prop->flags;
		return prop->value;
	}
	return NULL;
}

static bool
lilv_state_has_path(const char* path, const void* state)
{
	return lilv_state_rel2abs((const LilvState*)state, path) != path;
}

static char*
make_path(LV2_State_Make_Path_Handle handle, const char* path)
{
	LilvState* state = (LilvState*)handle;
	lilv_mkdir_p(state->dir);

	return lilv_path_join(state->dir, path);
}

static char*
abstract_path(LV2_State_Map_Path_Handle handle,
	const char* abs_path)
{
	LilvState* state = (LilvState*)handle;
	char* path = NULL;
	char* real_path = lilv_realpath(abs_path);
	const PathMap key = { real_path, NULL };
	ZixTreeIter* iter = NULL;

	if (abs_path[0] == '\0') {
		return lilv_strdup(abs_path);
	}
	else if (!zix_tree_find(state->abs2rel, &key, &iter)) {
		// Already mapped path in a previous call
		PathMap* pm = (PathMap*)zix_tree_get(iter);
		free(real_path);
		return lilv_strdup(pm->rel);
	}
	else if (lilv_path_is_child(real_path, state->dir)) {
		// File in state directory (loaded, or created by plugin during save)
		path = lilv_path_relative_to(real_path, state->dir);
	}
	else if (lilv_path_is_child(real_path, state->scratch_dir)) {
		// File created by plugin earlier
		path = lilv_path_relative_to(real_path, state->scratch_dir);
		if (state->copy_dir) {
			int st = lilv_mkdir_p(state->copy_dir);
			if (st) {
				LILV_ERRORF("Error creating directory %s (%s)\n",
					state->copy_dir, strerror(st));
			}

			char* cpath = lilv_path_join(state->copy_dir, path);
			char* copy = lilv_get_latest_copy(real_path, cpath);
			if (!copy || !lilv_file_equals(real_path, copy)) {
				// No recent enough copy, make a new one
				free(copy);
				copy = lilv_find_free_path(cpath, lilv_path_exists, NULL);
				if ((st = lilv_copy_file(real_path, copy))) {
					LILV_ERRORF("Error copying state file %s (%s)\n",
						copy, strerror(st));
				}
			}
			free(real_path);
			free(cpath);

			// Refer to the latest copy in plugin state
			real_path = copy;
		}
	}
	else if (state->link_dir) {
		// New path outside state directory, make a link
		const char* slash = strrchr(real_path, '/');
		const char* name = slash ? (slash + 1) : real_path;

		// Find a free name in the (virtual) state directory
		path = lilv_find_free_path(name, lilv_state_has_path, state);
	}
	else {
		// No link directory, preserve absolute path
		path = lilv_strdup(abs_path);
	}

	// Add record to path mapping
	PathMap* pm = (PathMap*)malloc(sizeof(PathMap));
	pm->abs = real_path;
	pm->rel = lilv_strdup(path);
	zix_tree_insert(state->abs2rel, pm, NULL);
	zix_tree_insert(state->rel2abs, pm, NULL);

	return path;
}

static char*
absolute_path(LV2_State_Map_Path_Handle handle,
	const char* state_path)
{
	LilvState* state = (LilvState*)handle;
	char* path = NULL;
	if (lilv_path_is_absolute(state_path)) {
		// Absolute path, return identical path
		path = lilv_strdup(state_path);
	}
	else if (state->dir) {
		// Relative path inside state directory
		path = lilv_path_join(state->dir, state_path);
	}
	else {
		// State has not been saved, unmap
		path = lilv_strdup(lilv_state_rel2abs(state, state_path));
	}

	return path;
}

/** Return a new features array with built-in features added to `features`. */
static const LV2_Feature**
add_features(const LV2_Feature* const* features,
	const LV2_Feature* map,
	const LV2_Feature* make,
	const LV2_Feature* free)
{
	size_t n_features = 0;
	for (; features && features[n_features]; ++n_features) {}

	const LV2_Feature** ret = (const LV2_Feature**)calloc(
		n_features + 4, sizeof(LV2_Feature*));

	if (features) {
		memcpy(ret, features, n_features * sizeof(LV2_Feature*));
	}

	size_t i = n_features;
	if (map) {
		ret[i++] = map;
	}
	if (make) {
		ret[i++] = make;
	}
	if (free) {
		ret[i++] = free;
	}

	return ret;
}

static char*
absolute_dir(const char* path)
{
	char* abs_path = lilv_path_absolute(path);
	char* base = lilv_path_join(abs_path, NULL);
	free(abs_path);
	return base;
}

static const char*
state_strerror(LV2_State_Status st)
{
	switch (st) {
	case LV2_STATE_SUCCESS:         return "Completed successfully";
	case LV2_STATE_ERR_BAD_TYPE:    return "Unsupported type";
	case LV2_STATE_ERR_BAD_FLAGS:   return "Unsupported flags";
	case LV2_STATE_ERR_NO_FEATURE:  return "Missing features";
	case LV2_STATE_ERR_NO_PROPERTY: return "Missing property";
	default:                        return "Unknown error";
	}
}

static void
lilv_free_path(LV2_State_Free_Path_Handle handle, char* path)
{
	lilv_free(path);
}

LilvState*
lilv_state_new_from_instance(const LilvPlugin* plugin,
	LilvInstance* instance,
	LV2_URID_Map* map,
	const char* scratch_dir,
	const char* copy_dir,
	const char* link_dir,
	const char* save_dir,
	LilvGetPortValueFunc       get_value,
	void* user_data,
	uint32_t                   flags,
	const LV2_Feature* const* features)
{
	const LV2_Feature** sfeatures = NULL;
	LilvWorld* const    world = plugin->world;
	LilvState* const    state = (LilvState*)calloc(1, sizeof(LilvState));
	state->plugin_uri = lilv_node_duplicate(lilv_plugin_get_uri(plugin));
	state->abs2rel = zix_tree_new(false, abs_cmp, NULL, path_rel_free);
	state->rel2abs = zix_tree_new(false, rel_cmp, NULL, NULL);
	state->scratch_dir = scratch_dir ? absolute_dir(scratch_dir) : NULL;
	state->copy_dir = copy_dir ? absolute_dir(copy_dir) : NULL;
	state->link_dir = link_dir ? absolute_dir(link_dir) : NULL;
	state->dir = save_dir ? absolute_dir(save_dir) : NULL;
	state->atom_Path = map->map(map->handle, LV2_ATOM__Path);

	LV2_State_Map_Path  pmap = { state, abstract_path, absolute_path };
	LV2_Feature         pmap_feature = { LV2_STATE__mapPath, &pmap };
	LV2_State_Make_Path pmake = { state, make_path };
	LV2_Feature         pmake_feature = { LV2_STATE__makePath, &pmake };
	LV2_State_Free_Path pfree = { NULL, lilv_free_path };
	LV2_Feature         pfree_feature = { LV2_STATE__freePath, &pfree };
	features = sfeatures = add_features(features, &pmap_feature,
		save_dir ? &pmake_feature : NULL,
		&pfree_feature);

	// Store port values
	if (get_value) {
		LilvNode* lv2_ControlPort = lilv_new_uri(world, LILV_URI_CONTROL_PORT);
		LilvNode* lv2_InputPort = lilv_new_uri(world, LILV_URI_INPUT_PORT);
		for (uint32_t i = 0; i < plugin->num_ports; ++i) {
			const LilvPort* const port = plugin->ports[i];
			if (lilv_port_is_a(plugin, port, lv2_ControlPort)
				&& lilv_port_is_a(plugin, port, lv2_InputPort)) {
				uint32_t size, type;
				const char* sym = lilv_node_as_string(port->symbol);
				const void* value = get_value(sym, user_data, &size, &type);
				append_port_value(state, sym, value, size, type);
			}
		}
		lilv_node_free(lv2_ControlPort);
		lilv_node_free(lv2_InputPort);
	}

	// Store properties
	const LV2_Descriptor* desc = instance->lv2_descriptor;
	const LV2_State_Interface* iface = (desc->extension_data)
		? (const LV2_State_Interface*)desc->extension_data(LV2_STATE__interface)
		: NULL;

	if (iface) {
		LV2_State_Status st = iface->save(
			instance->lv2_handle, store_callback, state, flags, features);
		if (st) {
			LILV_ERRORF("Error saving plugin state: %s\n", state_strerror(st));
			free(state->props.props);
			state->props.props = NULL;
			state->props.n = 0;
		}
		else {
			qsort(state->props.props, state->props.n, sizeof(Property), property_cmp);
		}
	}

	qsort(state->values, state->n_values, sizeof(PortValue), value_cmp);

	free(sfeatures);
	return state;
}

void
lilv_state_emit_port_values(const LilvState* state,
	LilvSetPortValueFunc set_value,
	void* user_data)
{
	for (uint32_t i = 0; i < state->n_values; ++i) {
		const PortValue* value = &state->values[i];
		const LV2_Atom* atom = value->atom;
		set_value(value->symbol, user_data, atom + 1, atom->size, atom->type);
	}
}

void
lilv_state_restore(const LilvState* state,
	LilvInstance* instance,
	LilvSetPortValueFunc       set_value,
	void* user_data,
	uint32_t                   flags,
	const LV2_Feature* const* features)
{
	if (!state) {
		LILV_ERROR("lilv_state_restore() called on NULL state\n");
		return;
	}

	LV2_State_Map_Path map_path = {
		(LilvState*)state, abstract_path, absolute_path };
	LV2_Feature map_feature = { LV2_STATE__mapPath, &map_path };

	LV2_State_Free_Path free_path = { NULL, lilv_free_path };
	LV2_Feature         free_feature = { LV2_STATE__freePath, &free_path };

	if (instance) {
		const LV2_Descriptor* desc = instance->lv2_descriptor;
		if (desc->extension_data) {
			const LV2_State_Interface* iface = (const LV2_State_Interface*)
				desc->extension_data(LV2_STATE__interface);

			if (iface && iface->restore) {
				const LV2_Feature** sfeatures = add_features(
					features, &map_feature, NULL, &free_feature);

				iface->restore(instance->lv2_handle, retrieve_callback,
					(LV2_State_Handle)state, flags, sfeatures);

				free(sfeatures);
			}
		}
	}


	if (set_value) {
		lilv_state_emit_port_values(state, set_value, user_data);
	}
}

static void
set_state_dir_from_model(LilvState* state, const SordNode* graph)
{
	if (!state->dir && graph) {
		const char* uri = (const char*)sord_node_get_string(graph);
		char* path = lilv_file_uri_parse(uri, NULL);

		state->dir = lilv_dir_path(path);
		free(path);
	}
	assert(!state->dir || lilv_path_is_absolute(state->dir));
}

static LilvState*
new_state_from_model(LilvWorld* world,
	LV2_URID_Map* map,
	SordModel* model,
	const SordNode* node,
	const char* dir)
{
	// Check that we know at least something about this state subject
	if (!sord_ask(model, node, 0, 0, 0)) {
		return NULL;
	}

	// Allocate state
	LilvState* const state = (LilvState*)calloc(1, sizeof(LilvState));
	state->dir = lilv_dir_path(dir);
	state->atom_Path = map->map(map->handle, LV2_ATOM__Path);
	state->uri = lilv_node_new_from_node(world, node);

	// Get the plugin URI this state applies to
	SordIter* i = sord_search(model, node, world->uris.lv2_appliesTo, 0, 0);
	if (i) {
		const SordNode* object = sord_iter_get_node(i, SORD_OBJECT);
		const SordNode* graph = sord_iter_get_node(i, SORD_GRAPH);
		state->plugin_uri = lilv_node_new_from_node(world, object);
		set_state_dir_from_model(state, graph);
		sord_iter_free(i);
	}
	else if (sord_ask(model,
		node,
		world->uris.rdf_a,
		world->uris.lv2_Plugin, 0)) {
		// Loading plugin description as state (default state)
		state->plugin_uri = lilv_node_new_from_node(world, node);
	}
	else {
		LILV_ERRORF("State %s missing lv2:appliesTo property\n",
			sord_node_get_string(node));
	}

	// Get the state label
	i = sord_search(model, node, world->uris.rdfs_label, NULL, NULL);
	if (i) {
		const SordNode* object = sord_iter_get_node(i, SORD_OBJECT);
		const SordNode* graph = sord_iter_get_node(i, SORD_GRAPH);
		state->label = lilv_strdup((const char*)sord_node_get_string(object));
		set_state_dir_from_model(state, graph);
		sord_iter_free(i);
	}

	Sratom* sratom = sratom_new(map);
	SerdChunk      chunk = { NULL, 0 };
	LV2_Atom_Forge forge;
	lv2_atom_forge_init(&forge, map);
	lv2_atom_forge_set_sink(
		&forge, sratom_forge_sink, sratom_forge_deref, &chunk);

	// Get port values
	SordIter* ports = sord_search(model, node, world->uris.lv2_port, 0, 0);
	FOREACH_MATCH(ports) {
		const SordNode* port = sord_iter_get_node(ports, SORD_OBJECT);

		SordNode* label = sord_get(model, port, world->uris.rdfs_label, 0, 0);
		SordNode* symbol = sord_get(model, port, world->uris.lv2_symbol, 0, 0);
		SordNode* value = sord_get(model, port, world->uris.pset_value, 0, 0);
		if (!value) {
			value = sord_get(model, port, world->uris.lv2_default, 0, 0);
		}
		if (!symbol) {
			LILV_ERRORF("State `%s' port missing symbol.\n",
				sord_node_get_string(node));
		}
		else if (value) {
			chunk.len = 0;
			sratom_read(sratom, &forge, world->world, model, value);
			const LV2_Atom* atom = (const LV2_Atom*)chunk.buf;

			append_port_value(state,
				(const char*)sord_node_get_string(symbol),
				LV2_ATOM_BODY_CONST(atom),
				atom->size, atom->type);

			if (label) {
				lilv_state_set_label(state,
					(const char*)sord_node_get_string(label));
			}
		}
		sord_node_free(world->world, value);
		sord_node_free(world->world, symbol);
		sord_node_free(world->world, label);
	}
	sord_iter_free(ports);

	// Get properties
	SordNode* statep = sord_new_uri(world->world, USTR(LV2_STATE__state));
	SordNode* state_node = sord_get(model, node, statep, NULL, NULL);
	if (state_node) {
		SordIter* props = sord_search(model, state_node, 0, 0, 0);
		FOREACH_MATCH(props) {
			const SordNode* p = sord_iter_get_node(props, SORD_PREDICATE);
			const SordNode* o = sord_iter_get_node(props, SORD_OBJECT);
			const char* key = (const char*)sord_node_get_string(p);

			chunk.len = 0;
			lv2_atom_forge_set_sink(
				&forge, sratom_forge_sink, sratom_forge_deref, &chunk);

			sratom_read(sratom, &forge, world->world, model, o);
			const LV2_Atom* atom = (const LV2_Atom*)chunk.buf;
			uint32_t        flags = LV2_STATE_IS_POD | LV2_STATE_IS_PORTABLE;
			Property        prop = { NULL, 0, 0, 0, flags };

			prop.key = map->map(map->handle, key);
			prop.type = atom->type;
			prop.size = atom->size;
			prop.value = malloc(atom->size);
			memcpy(prop.value, LV2_ATOM_BODY_CONST(atom), atom->size);
			if (atom->type == forge.Path) {
				prop.flags = LV2_STATE_IS_POD;
			}

			if (prop.value) {
				state->props.props = (Property*)realloc(
					state->props.props, (++state->props.n) * sizeof(Property));
				state->props.props[state->props.n - 1] = prop;
			}
		}
		sord_iter_free(props);
	}
	sord_node_free(world->world, state_node);
	sord_node_free(world->world, statep);

	serd_free((void*)chunk.buf);
	sratom_free(sratom);

	if (state->props.props) {
		qsort(state->props.props, state->props.n, sizeof(Property), property_cmp);
	}
	if (state->values) {
		qsort(state->values, state->n_values, sizeof(PortValue), value_cmp);
	}

	return state;
}

LilvState*
lilv_state_new_from_world(LilvWorld* world,
	LV2_URID_Map* map,
	const LilvNode* node)
{
	if (!lilv_node_is_uri(node) && !lilv_node_is_blank(node)) {
		LILV_ERRORF("Subject `%s' is not a URI or blank node.\n",
			lilv_node_as_string(node));
		return NULL;
	}

	return new_state_from_model(world, map, world->model, node->node, NULL);
}

LilvState*
lilv_state_new_from_file(LilvWorld* world,
	LV2_URID_Map* map,
	const LilvNode* subject,
	const char* path)
{
	if (subject && !lilv_node_is_uri(subject)
		&& !lilv_node_is_blank(subject)) {
		LILV_ERRORF("Subject `%s' is not a URI or blank node.\n",
			lilv_node_as_string(subject));
		return NULL;
	}

	uint8_t* abs_path = (uint8_t*)lilv_path_absolute(path);
	SerdNode    node = serd_node_new_file_uri(abs_path, NULL, NULL, true);
	SerdEnv* env = serd_env_new(&node);
	SordModel* model = sord_new(world->world, SORD_SPO, false);
	SerdReader* reader = sord_new_reader(model, env, SERD_TURTLE, NULL);

	serd_reader_read_file(reader, node.buf);

	SordNode* subject_node = (subject)
		? subject->node
		: sord_node_from_serd_node(world->world, env, &node, NULL, NULL);

	char* dirname = lilv_dirname(path);
	char* real_path = lilv_realpath(dirname);
	char* dir_path = lilv_dir_path(real_path);
	LilvState* state =
		new_state_from_model(world, map, model, subject_node, dir_path);
	free(dir_path);
	free(real_path);
	free(dirname);

	serd_node_free(&node);
	free(abs_path);
	serd_reader_free(reader);
	sord_free(model);
	serd_env_free(env);
	return state;
}

static void
set_prefixes(SerdEnv* env)
{
#define SET_PSET(e, p, u) serd_env_set_prefix_from_strings(e, p, u)
	SET_PSET(env, USTR("atom"), USTR(LV2_ATOM_PREFIX));
	SET_PSET(env, USTR("lv2"), USTR(LV2_CORE_PREFIX));
	SET_PSET(env, USTR("pset"), USTR(LV2_PRESETS_PREFIX));
	SET_PSET(env, USTR("rdf"), USTR(LILV_NS_RDF));
	SET_PSET(env, USTR("rdfs"), USTR(LILV_NS_RDFS));
	SET_PSET(env, USTR("state"), USTR(LV2_STATE_PREFIX));
	SET_PSET(env, USTR("xsd"), USTR(LILV_NS_XSD));
}

LilvState*
lilv_state_new_from_string(LilvWorld* world,
	LV2_URID_Map* map,
	const char* str)
{
	if (!str) {
		return NULL;
	}

	SerdNode    base = SERD_NODE_NULL;
	SerdEnv* env = serd_env_new(&base);
	SordModel* model = sord_new(world->world, SORD_SPO | SORD_OPS, false);
	SerdReader* reader = sord_new_reader(model, env, SERD_TURTLE, NULL);

	set_prefixes(env);
	serd_reader_read_string(reader, USTR(str));

	SordNode* o = sord_new_uri(world->world, USTR(LV2_PRESETS__Preset));
	SordNode* s = sord_get(model, NULL, world->uris.rdf_a, o, NULL);

	LilvState* state = new_state_from_model(world, map, model, s, NULL);

	sord_node_free(world->world, s);
	sord_node_free(world->world, o);
	serd_reader_free(reader);
	sord_free(model);
	serd_env_free(env);

	return state;
}

static SerdWriter*
ttl_writer(SerdSink sink, void* stream, const SerdNode* base, SerdEnv** new_env)
{
	SerdURI base_uri = SERD_URI_NULL;
	if (base && base->buf) {
		serd_uri_parse(base->buf, &base_uri);
	}

	SerdEnv* env = *new_env ? *new_env : serd_env_new(base);
	set_prefixes(env);

	SerdWriter* writer = serd_writer_new(
		SERD_TURTLE,
		(SerdStyle)(SERD_STYLE_RESOLVED |
			SERD_STYLE_ABBREVIATED |
			SERD_STYLE_CURIED),
		env,
		&base_uri,
		sink,
		stream);

	if (!*new_env) {
		*new_env = env;
	}

	return writer;
}

static SerdWriter*
ttl_file_writer(FILE* fd, const SerdNode* node, SerdEnv** env)
{
	SerdWriter* writer = ttl_writer(serd_file_sink, fd, node, env);

	fseek(fd, 0, SEEK_END);
	if (ftell(fd) == 0) {
		serd_env_foreach(*env, (SerdPrefixSink)serd_writer_set_prefix, writer);
	}
	else {
		fprintf(fd, "\n");
	}

	return writer;
}

static void
add_to_model(SordWorld* world,
	SerdEnv* env,
	SordModel* model,
	const SerdNode s,
	const SerdNode p,
	const SerdNode o)
{
	SordNode* ss = sord_node_from_serd_node(world, env, &s, NULL, NULL);
	SordNode* sp = sord_node_from_serd_node(world, env, &p, NULL, NULL);
	SordNode* so = sord_node_from_serd_node(world, env, &o, NULL, NULL);

	SordQuad quad = { ss, sp, so, NULL };
	sord_add(model, quad);

	sord_node_free(world, ss);
	sord_node_free(world, sp);
	sord_node_free(world, so);
}

static void
remove_manifest_entry(SordWorld* world, SordModel* model, const char* subject)
{
	SordNode* s = sord_new_uri(world, USTR(subject));
	SordIter* i = sord_search(model, s, NULL, NULL, NULL);
	while (!sord_iter_end(i)) {
		sord_erase(model, i);
	}
	sord_iter_free(i);
	sord_node_free(world, s);
}

static int
write_manifest(LilvWorld* world,
	SerdEnv* env,
	SordModel* model,
	const SerdNode* file_uri)
{
	char* const path = (char*)serd_file_uri_parse(file_uri->buf, NULL);
	FILE* const wfd = fopen(path, "w");
	if (!wfd) {
		LILV_ERRORF("Failed to open %s for writing (%s)\n",
			path, strerror(errno));

		serd_free(path);
		return 1;
	}

	SerdWriter* writer = ttl_file_writer(wfd, file_uri, &env);
	sord_write(model, writer, NULL);
	serd_writer_free(writer);
	fclose(wfd);
	serd_free(path);
	return 0;
}

static int
add_state_to_manifest(LilvWorld* lworld,
	const LilvNode* plugin_uri,
	const char* manifest_path,
	const char* state_uri,
	const char* state_path)
{
	SordWorld* world = lworld->world;
	SerdNode    manifest = serd_node_new_file_uri(USTR(manifest_path), 0, 0, 1);
	SerdNode    file = serd_node_new_file_uri(USTR(state_path), 0, 0, 1);
	SerdEnv* env = serd_env_new(&manifest);
	SordModel* model = sord_new(world, SORD_SPO, false);

	FILE* rfd = fopen(manifest_path, "r");
	if (rfd) {
		// Read manifest into model
		SerdReader* reader = sord_new_reader(model, env, SERD_TURTLE, NULL);
		lilv_flock(rfd, true);
		serd_reader_read_file_handle(reader, rfd, manifest.buf);
		serd_reader_free(reader);
	}

	// Choose state URI (use file URI if not given)
	if (!state_uri) {
		state_uri = (const char*)file.buf;
	}

	// Remove any existing manifest entries for this state
	remove_manifest_entry(world, model, state_uri);

	// Add manifest entry for this state to model
	SerdNode s = serd_node_from_string(SERD_URI, USTR(state_uri));

	// <state> a pset:Preset
	add_to_model(world, env, model,
		s,
		serd_node_from_string(SERD_URI, USTR(LILV_NS_RDF "type")),
		serd_node_from_string(SERD_URI, USTR(LV2_PRESETS__Preset)));

	// <state> a pset:Preset
	add_to_model(world, env, model,
		s,
		serd_node_from_string(SERD_URI, USTR(LILV_NS_RDF "type")),
		serd_node_from_string(SERD_URI, USTR(LV2_PRESETS__Preset)));

	// <state> rdfs:seeAlso <file>
	add_to_model(world, env, model,
		s,
		serd_node_from_string(SERD_URI, USTR(LILV_NS_RDFS "seeAlso")),
		file);

	// <state> lv2:appliesTo <plugin>
	add_to_model(world, env, model,
		s,
		serd_node_from_string(SERD_URI, USTR(LV2_CORE__appliesTo)),
		serd_node_from_string(SERD_URI,
			USTR(lilv_node_as_string(plugin_uri))));

	// Write manifest model to file
	write_manifest(lworld, env, model, &manifest);

	sord_free(model);
	serd_node_free(&file);
	serd_node_free(&manifest);
	serd_env_free(env);

	if (rfd) {
		lilv_flock(rfd, false);
		fclose(rfd);
	}

	return 0;
}

static bool
link_exists(const char* path, const void* data)
{
	const char* target = (const char*)data;
	if (!lilv_path_exists(path, NULL)) {
		return false;
	}
	char* real_path = lilv_realpath(path);
	bool  matches = !strcmp(real_path, target);
	free(real_path);
	return !matches;
}

static int
maybe_symlink(const char* oldpath, const char* newpath)
{
	return link_exists(newpath, oldpath) ? 0 : lilv_symlink(oldpath, newpath);
}

static void
write_property_array(const LilvState* state,
	const PropertyArray* array,
	Sratom* sratom,
	uint32_t             flags,
	const SerdNode* subject,
	LV2_URID_Unmap* unmap,
	const char* dir)
{
	for (uint32_t i = 0; i < array->n; ++i) {
		Property* prop = &array->props[i];
		const char* key = unmap->unmap(unmap->handle, prop->key);

		const SerdNode p = serd_node_from_string(SERD_URI, USTR(key));
		if (prop->type == state->atom_Path && !dir) {
			const char* path = (const char*)prop->value;
			const char* abs_path = lilv_state_rel2abs(state, path);
			LILV_WARNF("Writing absolute path %s\n", abs_path);
			sratom_write(sratom, unmap, flags,
				subject, &p, prop->type,
				strlen(abs_path) + 1, abs_path);
		}
		else if (prop->flags & LV2_STATE_IS_POD ||
			prop->type == state->atom_Path) {
			sratom_write(sratom, unmap, flags,
				subject, &p, prop->type, prop->size, prop->value);
		}
		else {
			LILV_WARNF("Lost non-POD property <%s> on save\n", key);
		}
	}
}

static int
lilv_state_write(LilvWorld* world,
	LV2_URID_Map* map,
	LV2_URID_Unmap* unmap,
	const LilvState* state,
	SerdWriter* writer,
	const char* uri,
	const char* dir)
{
	SerdNode lv2_appliesTo = serd_node_from_string(
		SERD_CURIE, USTR("lv2:appliesTo"));

	const SerdNode* plugin_uri = sord_node_to_serd_node(
		state->plugin_uri->node);

	SerdNode subject = serd_node_from_string(SERD_URI, USTR(uri ? uri : ""));

	// <subject> a pset:Preset
	SerdNode p = serd_node_from_string(SERD_URI, USTR(LILV_NS_RDF "type"));
	SerdNode o = serd_node_from_string(SERD_URI, USTR(LV2_PRESETS__Preset));
	serd_writer_write_statement(writer, 0, NULL,
		&subject, &p, &o, NULL, NULL);

	// <subject> lv2:appliesTo <http://example.org/plugin>
	serd_writer_write_statement(writer, 0, NULL,
		&subject,
		&lv2_appliesTo,
		plugin_uri, NULL, NULL);

	// <subject> rdfs:label label
	if (state->label) {
		p = serd_node_from_string(SERD_URI, USTR(LILV_NS_RDFS "label"));
		o = serd_node_from_string(SERD_LITERAL, USTR(state->label));
		serd_writer_write_statement(writer, 0,
			NULL, &subject, &p, &o, NULL, NULL);
	}

	SerdEnv* env = serd_writer_get_env(writer);
	const SerdNode* base = serd_env_get_base_uri(env, NULL);

	Sratom* sratom = sratom_new(map);
	sratom_set_sink(sratom, (const char*)base->buf,
		(SerdStatementSink)serd_writer_write_statement,
		(SerdEndSink)serd_writer_end_anon,
		writer);

	// Write metadata
	sratom_set_pretty_numbers(sratom, false);  // Use precise types
	write_property_array(state, &state->metadata, sratom, 0,
		&subject, unmap, dir);

	// Write port values
	sratom_set_pretty_numbers(sratom, true);  // Use pretty numbers
	for (uint32_t i = 0; i < state->n_values; ++i) {
		PortValue* const value = &state->values[i];

		const SerdNode port = serd_node_from_string(
			SERD_BLANK, USTR(value->symbol));

		// <> lv2:port _:symbol
		p = serd_node_from_string(SERD_URI, USTR(LV2_CORE__port));
		serd_writer_write_statement(writer, SERD_ANON_O_BEGIN,
			NULL, &subject, &p, &port, NULL, NULL);

		// _:symbol lv2:symbol "symbol"
		p = serd_node_from_string(SERD_URI, USTR(LV2_CORE__symbol));
		o = serd_node_from_string(SERD_LITERAL, USTR(value->symbol));
		serd_writer_write_statement(writer, SERD_ANON_CONT,
			NULL, &port, &p, &o, NULL, NULL);

		// _:symbol pset:value value
		p = serd_node_from_string(SERD_URI, USTR(LV2_PRESETS__value));
		sratom_write(sratom, unmap, SERD_ANON_CONT, &port, &p,
			value->atom->type, value->atom->size, value->atom + 1);

		serd_writer_end_anon(writer, &port);
	}

	// Write properties
	const SerdNode body = serd_node_from_string(SERD_BLANK, USTR("body"));
	if (state->props.n > 0) {
		p = serd_node_from_string(SERD_URI, USTR(LV2_STATE__state));
		serd_writer_write_statement(writer, SERD_ANON_O_BEGIN, NULL,
			&subject, &p, &body, NULL, NULL);
	}
	sratom_set_pretty_numbers(sratom, false);  // Use precise types
	write_property_array(state, &state->props, sratom, SERD_ANON_CONT,
		&body, unmap, dir);

	if (state->props.n > 0) {
		serd_writer_end_anon(writer, &body);
	}

	sratom_free(sratom);
	return 0;
}

static void
lilv_state_make_links(const LilvState* state, const char* dir)
{
	// Create symlinks to files
	for (ZixTreeIter* i = zix_tree_begin(state->abs2rel);
		i != zix_tree_end(state->abs2rel);
		i = zix_tree_iter_next(i)) {
		const PathMap* pm = (const PathMap*)zix_tree_get(i);

		char* path = lilv_path_join(dir, pm->rel);
		if (lilv_path_is_child(pm->abs, state->copy_dir)
			&& strcmp(state->copy_dir, dir)) {
			// Link directly to snapshot in the copy directory
			char* target = lilv_path_relative_to(pm->abs, dir);
			maybe_symlink(target, path);
			free(target);
		}
		else if (!lilv_path_is_child(pm->abs, dir)) {
			const char* link_dir = state->link_dir ? state->link_dir : dir;
			char* pat = lilv_path_join(link_dir, pm->rel);
			if (!strcmp(dir, link_dir)) {
				// Link directory is save directory, make link at exact path
				remove(pat);
				maybe_symlink(pm->abs, pat);
			}
			else {
				// Make a link in the link directory to external file
				char* lpath = lilv_find_free_path(pat, link_exists, pm->abs);
				if (!lilv_path_exists(lpath, NULL)) {
					lilv_symlink(pm->abs, lpath);
				}

				// Make a link in the save directory to the external link
				char* target = lilv_path_relative_to(lpath, dir);
				maybe_symlink(target, path);
				free(target);
				free(lpath);
			}
			free(pat);
		}
		free(path);
	}
}

int
lilv_state_save(LilvWorld* world,
	LV2_URID_Map* map,
	LV2_URID_Unmap* unmap,
	const LilvState* state,
	const char* uri,
	const char* dir,
	const char* filename)
{
	if (!filename || !dir || lilv_mkdir_p(dir)) {
		return 1;
	}

	char* abs_dir = absolute_dir(dir);
	char* const path = lilv_path_join(abs_dir, filename);
	FILE* fd = fopen(path, "w");
	if (!fd) {
		LILV_ERRORF("Failed to open %s (%s)\n", path, strerror(errno));
		free(abs_dir);
		free(path);
		return 4;
	}

	// Create symlinks to files if necessary
	lilv_state_make_links(state, abs_dir);

	// Write state to Turtle file
	SerdNode    file = serd_node_new_file_uri(USTR(path), NULL, NULL, true);
	SerdNode    node = uri ? serd_node_from_string(SERD_URI, USTR(uri)) : file;
	SerdEnv* env = NULL;
	SerdWriter* ttl = ttl_file_writer(fd, &file, &env);
	int         ret = lilv_state_write(
		world, map, unmap, state, ttl, (const char*)node.buf, dir);

	// Set saved dir and uri (FIXME: const violation)
	free(state->dir);
	lilv_node_free(state->uri);
	((LilvState*)state)->dir = lilv_strdup(abs_dir);
	((LilvState*)state)->uri = lilv_new_uri(world, (const char*)node.buf);

	serd_node_free(&file);
	serd_writer_free(ttl);
	serd_env_free(env);
	fclose(fd);

	// Add entry to manifest
	char* const manifest = lilv_path_join(abs_dir, "manifest.ttl");
	add_state_to_manifest(world, state->plugin_uri, manifest, uri, path);

	free(manifest);
	free(abs_dir);
	free(path);
	return ret;
}

char*
lilv_state_to_string(LilvWorld* world,
	LV2_URID_Map* map,
	LV2_URID_Unmap* unmap,
	const LilvState* state,
	const char* uri,
	const char* base_uri)
{
	if (!uri) {
		LILV_ERROR("Attempt to serialise state with no URI\n");
		return NULL;
	}

	SerdChunk   chunk = { NULL, 0 };
	SerdEnv* env = NULL;
	SerdNode    base = serd_node_from_string(SERD_URI, USTR(base_uri));
	SerdWriter* writer = ttl_writer(serd_chunk_sink, &chunk, &base, &env);

	lilv_state_write(world, map, unmap, state, writer, uri, NULL);

	serd_writer_free(writer);
	serd_env_free(env);
	char* str = (char*)serd_chunk_sink_finish(&chunk);
	char* result = lilv_strdup(str);
	serd_free(str);
	return result;
}

static void
try_unlink(const char* state_dir, const char* path)
{
	if (!strncmp(state_dir, path, strlen(state_dir))) {
		if (lilv_path_exists(path, NULL) && unlink(path)) {
			LILV_ERRORF("Failed to remove %s (%s)\n", path, strerror(errno));
		}
	}
}

int
lilv_state_delete(LilvWorld* world,
	const LilvState* state)
{
	if (!state->dir) {
		LILV_ERROR("Attempt to delete unsaved state\n");
		return -1;
	}

	LilvNode* bundle = lilv_new_file_uri(world, NULL, state->dir);
	LilvNode* manifest = lilv_world_get_manifest_uri(world, bundle);
	char* manifest_path = lilv_node_get_path(manifest, NULL);
	const bool has_manifest = lilv_path_exists(manifest_path, NULL);
	SordModel* model = sord_new(world->world, SORD_SPO, false);

	if (has_manifest) {
		// Read manifest into temporary local model
		SerdEnv* env = serd_env_new(sord_node_to_serd_node(manifest->node));
		SerdReader* ttl = sord_new_reader(model, env, SERD_TURTLE, NULL);
		serd_reader_read_file(ttl, USTR(manifest_path));
		serd_reader_free(ttl);
		serd_env_free(env);
	}

	if (state->uri) {
		SordNode* file = sord_get(
			model, state->uri->node, world->uris.rdfs_seeAlso, NULL, NULL);
		if (file) {
			// Remove state file
			const uint8_t* uri = sord_node_get_string(file);
			char* path = (char*)serd_file_uri_parse(uri, NULL);
			try_unlink(state->dir, path);
			serd_free(path);
		}

		// Remove any existing manifest entries for this state
		const char* state_uri_str = lilv_node_as_string(state->uri);
		remove_manifest_entry(world->world, model, state_uri_str);
		remove_manifest_entry(world->world, world->model, state_uri_str);
	}

	// Drop bundle from model
	lilv_world_unload_bundle(world, bundle);

	if (sord_num_quads(model) == 0) {
		// Manifest is empty, attempt to remove bundle entirely
		if (has_manifest) {
			try_unlink(state->dir, manifest_path);
		}

		// Remove all known files from state bundle
		if (state->abs2rel) {
			// State created from instance, get paths from map
			for (ZixTreeIter* i = zix_tree_begin(state->abs2rel);
				i != zix_tree_end(state->abs2rel);
				i = zix_tree_iter_next(i)) {
				const PathMap* pm = (const PathMap*)zix_tree_get(i);
				char* path = lilv_path_join(state->dir, pm->rel);
				try_unlink(state->dir, path);
				free(path);
			}
		}
		else {
			// State loaded from model, get paths from loaded properties
			for (uint32_t i = 0; i < state->props.n; ++i) {
				const Property* const p = &state->props.props[i];
				if (p->type == state->atom_Path) {
					try_unlink(state->dir, (const char*)p->value);
				}
			}
		}

		if (rmdir(state->dir)) {
			LILV_ERRORF("Failed to remove directory %s (%s)\n",
				state->dir, strerror(errno));
		}
	}
	else {
		// Still something in the manifest, update and reload bundle
		const SerdNode* manifest_node = sord_node_to_serd_node(manifest->node);
		SerdEnv* env = serd_env_new(manifest_node);

		write_manifest(world, env, model, manifest_node);
		lilv_world_load_bundle(world, bundle);
		serd_env_free(env);
	}

	sord_free(model);
	lilv_free(manifest_path);
	lilv_node_free(manifest);
	lilv_node_free(bundle);

	return 0;
}

static void
free_property_array(LilvState* state, PropertyArray* array)
{
	for (uint32_t i = 0; i < array->n; ++i) {
		Property* prop = &array->props[i];
		if ((prop->flags & LV2_STATE_IS_POD) ||
			prop->type == state->atom_Path) {
			free(prop->value);
		}
	}
	free(array->props);
}

void
lilv_state_free(LilvState* state)
{
	if (state) {
		free_property_array(state, &state->props);
		free_property_array(state, &state->metadata);
		for (uint32_t i = 0; i < state->n_values; ++i) {
			free(state->values[i].atom);
			free(state->values[i].symbol);
		}
		lilv_node_free(state->plugin_uri);
		lilv_node_free(state->uri);
		zix_tree_free(state->abs2rel);
		zix_tree_free(state->rel2abs);
		free(state->values);
		free(state->label);
		free(state->dir);
		free(state->scratch_dir);
		free(state->copy_dir);
		free(state->link_dir);
		free(state);
	}
}

bool
lilv_state_equals(const LilvState* a, const LilvState* b)
{
	if (!lilv_node_equals(a->plugin_uri, b->plugin_uri)
		|| (a->label && !b->label)
		|| (b->label && !a->label)
		|| (a->label && b->label && strcmp(a->label, b->label))
		|| a->props.n != b->props.n
		|| a->n_values != b->n_values) {
		return false;
	}

	for (uint32_t i = 0; i < a->n_values; ++i) {
		PortValue* const av = &a->values[i];
		PortValue* const bv = &b->values[i];
		if (av->atom->size != bv->atom->size ||
			av->atom->type != bv->atom->type ||
			strcmp(av->symbol, bv->symbol) ||
			memcmp(av->atom + 1, bv->atom + 1, av->atom->size)) {
			return false;
		}
	}

	for (uint32_t i = 0; i < a->props.n; ++i) {
		Property* const ap = &a->props.props[i];
		Property* const bp = &b->props.props[i];
		if (ap->key != bp->key
			|| ap->type != bp->type
			|| ap->flags != bp->flags) {
			return false;
		}
		else if (ap->type == a->atom_Path) {
			if (!lilv_file_equals(lilv_state_rel2abs(a, (char*)ap->value),
				lilv_state_rel2abs(b, (char*)bp->value))) {
				return false;
			}
		}
		else if (ap->size != bp->size
			|| memcmp(ap->value, bp->value, ap->size)) {
			return false;
		}
	}

	return true;
}

unsigned
lilv_state_get_num_properties(const LilvState* state)
{
	return state->props.n;
}

const LilvNode*
lilv_state_get_plugin_uri(const LilvState* state)
{
	return state->plugin_uri;
}

const LilvNode*
lilv_state_get_uri(const LilvState* state)
{
	return state->uri;
}

const char*
lilv_state_get_label(const LilvState* state)
{
	return state->label;
}

void
lilv_state_set_label(LilvState* state, const char* label)
{
	const size_t len = strlen(label);
	state->label = (char*)realloc(state->label, len + 1);
	memcpy(state->label, label, len + 1);
}

int
lilv_state_set_metadata(LilvState* state,
	uint32_t    key,
	const void* value,
	size_t      size,
	uint32_t    type,
	uint32_t    flags)
{
	append_property(state, &state->metadata, key, value, size, type, flags);
	return LV2_STATE_SUCCESS;
}


LilvUI*
lilv_ui_new(LilvWorld* world,
	LilvNode* uri,
	LilvNode* type_uri,
	LilvNode* binary_uri)
{
	assert(uri);
	assert(type_uri);
	assert(binary_uri);

	LilvUI* ui = (LilvUI*)malloc(sizeof(LilvUI));
	ui->world = world;
	ui->uri = uri;
	ui->binary_uri = binary_uri;

	// FIXME: kludge
	char* bundle = lilv_strdup(lilv_node_as_string(ui->binary_uri));
	char* last_slash = strrchr(bundle, '/') + 1;
	*last_slash = '\0';
	ui->bundle_uri = lilv_new_uri(world, bundle);
	free(bundle);

	ui->classes = lilv_nodes_new();
	zix_tree_insert((ZixTree*)ui->classes, type_uri, NULL);

	return ui;
}

void
lilv_ui_free(LilvUI* ui)
{
	lilv_node_free(ui->uri);
	lilv_node_free(ui->bundle_uri);
	lilv_node_free(ui->binary_uri);
	lilv_nodes_free(ui->classes);
	free(ui);
}

const LilvNode*
lilv_ui_get_uri(const LilvUI* ui)
{
	return ui->uri;
}

unsigned
lilv_ui_is_supported(const LilvUI* ui,
	LilvUISupportedFunc supported_func,
	const LilvNode* container_type,
	const LilvNode** ui_type)
{
	const LilvNodes* classes = lilv_ui_get_classes(ui);
	LILV_FOREACH(nodes, c, classes) {
		const LilvNode* type = lilv_nodes_get(classes, c);
		const unsigned  q = supported_func(lilv_node_as_uri(container_type),
			lilv_node_as_uri(type));
		if (q) {
			if (ui_type) {
				*ui_type = type;
			}
			return q;
		}
	}

	return 0;
}

const LilvNodes*
lilv_ui_get_classes(const LilvUI* ui)
{
	return ui->classes;
}

bool
lilv_ui_is_a(const LilvUI* ui, const LilvNode* class_uri)
{
	return lilv_nodes_contains(ui->classes, class_uri);
}

const LilvNode*
lilv_ui_get_bundle_uri(const LilvUI* ui)
{
	return ui->bundle_uri;
}

const LilvNode*
lilv_ui_get_binary_uri(const LilvUI* ui)
{
	return ui->binary_uri;
}


static int
lilv_world_drop_graph(LilvWorld* world, const SordNode* graph);

LilvWorld*
lilv_world_new(void)
{
	LilvWorld* world = (LilvWorld*)calloc(1, sizeof(LilvWorld));

	world->world = sord_world_new();
	if (!world->world) {
		goto fail;
	}

	world->model = sord_new(world->world, SORD_SPO | SORD_OPS, true);
	if (!world->model) {
		goto fail;
	}

	world->specs = NULL;
	world->plugin_classes = lilv_plugin_classes_new();
	world->plugins = lilv_plugins_new();
	world->zombies = lilv_plugins_new();
	world->loaded_files = zix_tree_new(
		false, lilv_resource_node_cmp, NULL, (ZixDestroyFunc)lilv_node_free);

	world->libs = zix_tree_new(false, lilv_lib_compare, NULL, NULL);

#define NS_DCTERMS "http://purl.org/dc/terms/"
#define NS_DYNMAN  "http://lv2plug.in/ns/ext/dynmanifest#"
#define NS_OWL     "http://www.w3.org/2002/07/owl#"

#define NEW_URI(uri) sord_new_uri(world->world, (const uint8_t*)(uri))

	world->uris.dc_replaces = NEW_URI(NS_DCTERMS   "replaces");
	world->uris.dman_DynManifest = NEW_URI(NS_DYNMAN    "DynManifest");
	world->uris.doap_name = NEW_URI(LILV_NS_DOAP "name");
	world->uris.lv2_Plugin = NEW_URI(LV2_CORE__Plugin);
	world->uris.lv2_Specification = NEW_URI(LV2_CORE__Specification);
	world->uris.lv2_appliesTo = NEW_URI(LV2_CORE__appliesTo);
	world->uris.lv2_binary = NEW_URI(LV2_CORE__binary);
	world->uris.lv2_default = NEW_URI(LV2_CORE__default);
	world->uris.lv2_designation = NEW_URI(LV2_CORE__designation);
	world->uris.lv2_extensionData = NEW_URI(LV2_CORE__extensionData);
	world->uris.lv2_index = NEW_URI(LV2_CORE__index);
	world->uris.lv2_latency = NEW_URI(LV2_CORE__latency);
	world->uris.lv2_maximum = NEW_URI(LV2_CORE__maximum);
	world->uris.lv2_microVersion = NEW_URI(LV2_CORE__microVersion);
	world->uris.lv2_minimum = NEW_URI(LV2_CORE__minimum);
	world->uris.lv2_minorVersion = NEW_URI(LV2_CORE__minorVersion);
	world->uris.lv2_name = NEW_URI(LV2_CORE__name);
	world->uris.lv2_optionalFeature = NEW_URI(LV2_CORE__optionalFeature);
	world->uris.lv2_port = NEW_URI(LV2_CORE__port);
	world->uris.lv2_portProperty = NEW_URI(LV2_CORE__portProperty);
	world->uris.lv2_reportsLatency = NEW_URI(LV2_CORE__reportsLatency);
	world->uris.lv2_requiredFeature = NEW_URI(LV2_CORE__requiredFeature);
	world->uris.lv2_symbol = NEW_URI(LV2_CORE__symbol);
	world->uris.lv2_prototype = NEW_URI(LV2_CORE__prototype);
	world->uris.owl_Ontology = NEW_URI(NS_OWL "Ontology");
	world->uris.pset_value = NEW_URI(LV2_PRESETS__value);
	world->uris.rdf_a = NEW_URI(LILV_NS_RDF  "type");
	world->uris.rdf_value = NEW_URI(LILV_NS_RDF  "value");
	world->uris.rdfs_Class = NEW_URI(LILV_NS_RDFS "Class");
	world->uris.rdfs_label = NEW_URI(LILV_NS_RDFS "label");
	world->uris.rdfs_seeAlso = NEW_URI(LILV_NS_RDFS "seeAlso");
	world->uris.rdfs_subClassOf = NEW_URI(LILV_NS_RDFS "subClassOf");
	world->uris.xsd_base64Binary = NEW_URI(LILV_NS_XSD  "base64Binary");
	world->uris.xsd_boolean = NEW_URI(LILV_NS_XSD  "boolean");
	world->uris.xsd_decimal = NEW_URI(LILV_NS_XSD  "decimal");
	world->uris.xsd_double = NEW_URI(LILV_NS_XSD  "double");
	world->uris.xsd_integer = NEW_URI(LILV_NS_XSD  "integer");
	world->uris.null_uri = NULL;

	world->lv2_plugin_class = lilv_plugin_class_new(
		world, NULL, world->uris.lv2_Plugin, "Plugin");
	assert(world->lv2_plugin_class);

	world->n_read_files = 0;
	world->opt.filter_language = true;
	world->opt.dyn_manifest = true;

	return world;

fail:
	/* keep on rockin' in the */ free(world);
	return NULL;
}

void
lilv_world_free(LilvWorld* world)
{
	if (!world) {
		return;
	}

	lilv_plugin_class_free(world->lv2_plugin_class);
	world->lv2_plugin_class = NULL;

	for (SordNode** n = (SordNode**)&world->uris; *n; ++n) {
		sord_node_free(world->world, *n);
	}

	for (LilvSpec* spec = world->specs; spec;) {
		LilvSpec* next = spec->next;
		sord_node_free(world->world, spec->spec);
		sord_node_free(world->world, spec->bundle);
		lilv_nodes_free(spec->data_uris);
		free(spec);
		spec = next;
	}
	world->specs = NULL;

	LILV_FOREACH(plugins, i, world->plugins) {
		const LilvPlugin* p = lilv_plugins_get(world->plugins, i);
		lilv_plugin_free((LilvPlugin*)p);
	}
	zix_tree_free((ZixTree*)world->plugins);
	world->plugins = NULL;

	LILV_FOREACH(plugins, i, world->zombies) {
		const LilvPlugin* p = lilv_plugins_get(world->zombies, i);
		lilv_plugin_free((LilvPlugin*)p);
	}
	zix_tree_free((ZixTree*)world->zombies);
	world->zombies = NULL;

	zix_tree_free((ZixTree*)world->loaded_files);
	world->loaded_files = NULL;

	zix_tree_free(world->libs);
	world->libs = NULL;

	zix_tree_free((ZixTree*)world->plugin_classes);
	world->plugin_classes = NULL;

	sord_free(world->model);
	world->model = NULL;

	sord_world_free(world->world);
	world->world = NULL;

	free(world->opt.lv2_path);
	free(world);
}

void
lilv_world_set_option(LilvWorld* world,
	const char* uri,
	const LilvNode* value)
{
	if (!strcmp(uri, LILV_OPTION_DYN_MANIFEST)) {
		if (lilv_node_is_bool(value)) {
			world->opt.dyn_manifest = lilv_node_as_bool(value);
			return;
		}
	}
	else if (!strcmp(uri, LILV_OPTION_FILTER_LANG)) {
		if (lilv_node_is_bool(value)) {
			world->opt.filter_language = lilv_node_as_bool(value);
			return;
		}
	}
	else if (!strcmp(uri, LILV_OPTION_LV2_PATH)) {
		if (lilv_node_is_string(value)) {
			world->opt.lv2_path = lilv_strdup(lilv_node_as_string(value));
			return;
		}
	}
	LILV_WARNF("Unrecognized or invalid option `%s'\n", uri);
}

LilvNodes*
lilv_world_find_nodes(LilvWorld* world,
	const LilvNode* subject,
	const LilvNode* predicate,
	const LilvNode* object)
{
	if (subject && !lilv_node_is_uri(subject) && !lilv_node_is_blank(subject)) {
		LILV_ERRORF("Subject `%s' is not a resource\n",
			sord_node_get_string(subject->node));
		return NULL;
	}
	else if (!predicate) {
		LILV_ERROR("Missing required predicate\n");
		return NULL;
	}
	else if (!lilv_node_is_uri(predicate)) {
		LILV_ERRORF("Predicate `%s' is not a URI\n",
			sord_node_get_string(predicate->node));
		return NULL;
	}
	else if (!subject && !object) {
		LILV_ERROR("Both subject and object are NULL\n");
		return NULL;
	}

	return lilv_world_find_nodes_internal(world,
		subject ? subject->node : NULL,
		predicate->node,
		object ? object->node : NULL);
}

LilvNode*
lilv_world_get(LilvWorld* world,
	const LilvNode* subject,
	const LilvNode* predicate,
	const LilvNode* object)
{
	SordNode* snode = sord_get(world->model,
		subject ? subject->node : NULL,
		predicate ? predicate->node : NULL,
		object ? object->node : NULL,
		NULL);
	LilvNode* lnode = lilv_node_new_from_node(world, snode);
	sord_node_free(world->world, snode);
	return lnode;
}

SordIter*
lilv_world_query_internal(LilvWorld* world,
	const SordNode* subject,
	const SordNode* predicate,
	const SordNode* object)
{
	return sord_search(world->model, subject, predicate, object, NULL);
}

bool
lilv_world_ask_internal(LilvWorld* world,
	const SordNode* subject,
	const SordNode* predicate,
	const SordNode* object)
{
	return sord_ask(world->model, subject, predicate, object, NULL);
}

bool
lilv_world_ask(LilvWorld* world,
	const LilvNode* subject,
	const LilvNode* predicate,
	const LilvNode* object)
{
	return sord_ask(world->model,
		subject ? subject->node : NULL,
		predicate ? predicate->node : NULL,
		object ? object->node : NULL,
		NULL);
}

SordModel*
lilv_world_filter_model(LilvWorld* world,
	SordModel* model,
	const SordNode* subject,
	const SordNode* predicate,
	const SordNode* object,
	const SordNode* graph)
{
	SordModel* results = sord_new(world->world, SORD_SPO, false);
	SordIter* i = sord_search(model, subject, predicate, object, graph);
	for (; !sord_iter_end(i); sord_iter_next(i)) {
		SordQuad quad;
		sord_iter_get(i, quad);
		sord_add(results, quad);
	}
	sord_iter_free(i);
	return results;
}

LilvNodes*
lilv_world_find_nodes_internal(LilvWorld* world,
	const SordNode* subject,
	const SordNode* predicate,
	const SordNode* object)
{
	return lilv_nodes_from_stream_objects(
		world,
		lilv_world_query_internal(world, subject, predicate, object),
		(object == NULL) ? SORD_OBJECT : SORD_SUBJECT);
}

static SerdNode
lilv_new_uri_relative_to_base(const uint8_t* uri_str,
	const uint8_t* base_uri_str)
{
	SerdURI base_uri;
	serd_uri_parse(base_uri_str, &base_uri);
	return serd_node_new_uri_from_string(uri_str, &base_uri, NULL);
}

const uint8_t*
lilv_world_blank_node_prefix(LilvWorld* world)
{
	static char str[32];
	snprintf(str, sizeof(str), "%d", world->n_read_files++);
	return (const uint8_t*)str;
}

/** Comparator for sequences (e.g. world->plugins). */
int
lilv_header_compare_by_uri(const void* a, const void* b, void* user_data)
{
	const struct LilvHeader* const header_a = (const struct LilvHeader*)a;
	const struct LilvHeader* const header_b = (const struct LilvHeader*)b;
	return strcmp(lilv_node_as_uri(header_a->uri),
		lilv_node_as_uri(header_b->uri));
}

/**
   Comparator for libraries (world->libs).

   Libraries do have a LilvHeader, but we must also compare the bundle to
   handle the case where the same library is loaded with different bundles, and
   consequently different contents (mainly plugins).
 */
int
lilv_lib_compare(const void* a, const void* b, void* user_data)
{
	const LilvLib* const lib_a = (const LilvLib*)a;
	const LilvLib* const lib_b = (const LilvLib*)b;
	int cmp = strcmp(lilv_node_as_uri(lib_a->uri),
		lilv_node_as_uri(lib_b->uri));
	return cmp ? cmp : strcmp(lib_a->bundle_path, lib_b->bundle_path);
}

/** Get an element of a collection of any object with an LilvHeader by URI. */
static ZixTreeIter*
lilv_collection_find_by_uri(const ZixTree* seq, const LilvNode* uri)
{
	ZixTreeIter* i = NULL;
	if (lilv_node_is_uri(uri)) {
		struct LilvHeader key = { NULL, (LilvNode*)uri };
		zix_tree_find(seq, &key, &i);
	}
	return i;
}

/** Get an element of a collection of any object with an LilvHeader by URI. */
struct LilvHeader*
	lilv_collection_get_by_uri(const ZixTree* seq, const LilvNode* uri)
{
	ZixTreeIter* const i = lilv_collection_find_by_uri(seq, uri);

	return i ? (struct LilvHeader*)zix_tree_get(i) : NULL;
}

static void
lilv_world_add_spec(LilvWorld* world,
	const SordNode* specification_node,
	const SordNode* bundle_node)
{
	LilvSpec* spec = (LilvSpec*)malloc(sizeof(LilvSpec));
	spec->spec = sord_node_copy(specification_node);
	spec->bundle = sord_node_copy(bundle_node);
	spec->data_uris = lilv_nodes_new();

	// Add all data files (rdfs:seeAlso)
	SordIter* files = sord_search(world->model,
		specification_node,
		world->uris.rdfs_seeAlso,
		NULL,
		NULL);
	FOREACH_MATCH(files) {
		const SordNode* file_node = sord_iter_get_node(files, SORD_OBJECT);
		zix_tree_insert((ZixTree*)spec->data_uris,
			lilv_node_new_from_node(world, file_node),
			NULL);
	}
	sord_iter_free(files);

	// Add specification to world specification list
	spec->next = world->specs;
	world->specs = spec;
}

static void
lilv_world_add_plugin(LilvWorld* world,
	const SordNode* plugin_node,
	const LilvNode* manifest_uri,
	void* dynmanifest,
	const SordNode* bundle)
{
	LilvNode* plugin_uri = lilv_node_new_from_node(world, plugin_node);
	ZixTreeIter* z = NULL;
	LilvPlugin* plugin = (LilvPlugin*)lilv_plugins_get_by_uri(
		world->plugins, plugin_uri);

	if (plugin) {
		// Existing plugin, if this is different bundle, ignore it
		// (use the first plugin found in LV2_PATH)
		const LilvNode* last_bundle = lilv_plugin_get_bundle_uri(plugin);
		const char* plugin_uri_str = lilv_node_as_uri(plugin_uri);
		if (sord_node_equals(bundle, last_bundle->node)) {
			LILV_WARNF("Reloading plugin <%s>\n", plugin_uri_str);
			plugin->loaded = false;
			lilv_node_free(plugin_uri);
		}
		else {
			LILV_WARNF("Duplicate plugin <%s>\n", plugin_uri_str);
			LILV_WARNF("... found in %s\n", lilv_node_as_string(last_bundle));
			LILV_WARNF("... and      %s (ignored)\n", sord_node_get_string(bundle));
			lilv_node_free(plugin_uri);
			return;
		}
	}
	else if ((z = lilv_collection_find_by_uri((const ZixTree*)world->zombies,
		plugin_uri))) {
		// Plugin bundle has been re-loaded, move from zombies to plugins
		plugin = (LilvPlugin*)zix_tree_get(z);
		zix_tree_remove((ZixTree*)world->zombies, z);
		zix_tree_insert((ZixTree*)world->plugins, plugin, NULL);
		lilv_node_free(plugin_uri);
		lilv_plugin_clear(plugin, lilv_node_new_from_node(world, bundle));
	}
	else {
		// Add new plugin to the world
		plugin = lilv_plugin_new(
			world, plugin_uri, lilv_node_new_from_node(world, bundle));

		// Add manifest as plugin data file (as if it were rdfs:seeAlso)
		zix_tree_insert((ZixTree*)plugin->data_uris,
			lilv_node_duplicate(manifest_uri),
			NULL);

		// Add plugin to world plugin sequence
		zix_tree_insert((ZixTree*)world->plugins, plugin, NULL);
	}


#ifdef LILV_DYN_MANIFEST
	// Set dynamic manifest library URI, if applicable
	if (dynmanifest) {
		plugin->dynmanifest = (LilvDynManifest*)dynmanifest;
		++((LilvDynManifest*)dynmanifest)->refs;
	}
#endif

	// Add all plugin data files (rdfs:seeAlso)
	SordIter* files = sord_search(world->model,
		plugin_node,
		world->uris.rdfs_seeAlso,
		NULL,
		NULL);
	FOREACH_MATCH(files) {
		const SordNode* file_node = sord_iter_get_node(files, SORD_OBJECT);
		zix_tree_insert((ZixTree*)plugin->data_uris,
			lilv_node_new_from_node(world, file_node),
			NULL);
	}
	sord_iter_free(files);
}

SerdStatus
lilv_world_load_graph(LilvWorld* world, SordNode* graph, const LilvNode* uri)
{
	const SerdNode* base = sord_node_to_serd_node(uri->node);
	SerdEnv* env = serd_env_new(base);
	SerdReader* reader = sord_new_reader(
		world->model, env, SERD_TURTLE, graph);

	const SerdStatus st = lilv_world_load_file(world, reader, uri);

	serd_env_free(env);
	serd_reader_free(reader);
	return st;
}

static void
lilv_world_load_dyn_manifest(LilvWorld* world,
	SordNode* bundle_node,
	const LilvNode* manifest)
{
#ifdef LILV_DYN_MANIFEST
	if (!world->opt.dyn_manifest) {
		return;
	}

	LV2_Dyn_Manifest_Handle handle = NULL;

	// ?dman a dynman:DynManifest bundle_node
	SordModel* model = lilv_world_filter_model(world,
		world->model,
		NULL,
		world->uris.rdf_a,
		world->uris.dman_DynManifest,
		bundle_node);
	SordIter* iter = sord_begin(model);
	for (; !sord_iter_end(iter); sord_iter_next(iter)) {
		const SordNode* dmanifest = sord_iter_get_node(iter, SORD_SUBJECT);

		// ?dman lv2:binary ?binary
		SordIter* binaries = sord_search(world->model,
			dmanifest,
			world->uris.lv2_binary,
			NULL,
			bundle_node);
		if (sord_iter_end(binaries)) {
			sord_iter_free(binaries);
			LILV_ERRORF("Dynamic manifest in <%s> has no binaries, ignored\n",
				sord_node_get_string(bundle_node));
			continue;
		}

		// Get binary path
		const SordNode* binary = sord_iter_get_node(binaries, SORD_OBJECT);
		const uint8_t* lib_uri = sord_node_get_string(binary);
		char* lib_path = lilv_file_uri_parse((const char*)lib_uri, 0);
		if (!lib_path) {
			LILV_ERROR("No dynamic manifest library path\n");
			sord_iter_free(binaries);
			continue;
		}

		// Open library
		dlerror();
		void* lib = dlopen(lib_path, RTLD_LAZY);
		if (!lib) {
			LILV_ERRORF("Failed to open dynmanifest library `%s' (%s)\n",
				lib_path, dlerror());
			sord_iter_free(binaries);
			lilv_free(lib_path);
			continue;
		}

		// Open dynamic manifest
		typedef int (*OpenFunc)(LV2_Dyn_Manifest_Handle*,
			const LV2_Feature* const*);
		OpenFunc dmopen = (OpenFunc)lilv_dlfunc(lib, "lv2_dyn_manifest_open");
		if (!dmopen || dmopen(&handle, &dman_features)) {
			LILV_ERRORF("No `lv2_dyn_manifest_open' in `%s'\n", lib_path);
			sord_iter_free(binaries);
			dlclose(lib);
			lilv_free(lib_path);
			continue;
		}

		// Get subjects (the data that would be in manifest.ttl)
		typedef int (*GetSubjectsFunc)(LV2_Dyn_Manifest_Handle, FILE*);
		GetSubjectsFunc get_subjects_func = (GetSubjectsFunc)lilv_dlfunc(
			lib, "lv2_dyn_manifest_get_subjects");
		if (!get_subjects_func) {
			LILV_ERRORF("No `lv2_dyn_manifest_get_subjects' in `%s'\n",
				lib_path);
			sord_iter_free(binaries);
			dlclose(lib);
			lilv_free(lib_path);
			continue;
		}

		LilvDynManifest* desc = (LilvDynManifest*)malloc(sizeof(LilvDynManifest));
		desc->bundle = lilv_node_new_from_node(world, bundle_node);
		desc->lib = lib;
		desc->handle = handle;
		desc->refs = 0;

		sord_iter_free(binaries);

		// Generate data file
		FILE* fd = tmpfile();
		get_subjects_func(handle, fd);
		rewind(fd);

		// Parse generated data file into temporary model
		// FIXME
		const SerdNode* base = sord_node_to_serd_node(dmanifest);
		SerdEnv* env = serd_env_new(base);
		SerdReader* reader = sord_new_reader(
			world->model, env, SERD_TURTLE, sord_node_copy(dmanifest));
		serd_reader_add_blank_prefix(reader,
			lilv_world_blank_node_prefix(world));
		serd_reader_read_file_handle(reader, fd,
			(const uint8_t*)"(dyn-manifest)");
		serd_reader_free(reader);
		serd_env_free(env);

		// Close (and automatically delete) temporary data file
		fclose(fd);

		// ?plugin a lv2:Plugin
		SordModel* plugins = lilv_world_filter_model(world,
			world->model,
			NULL,
			world->uris.rdf_a,
			world->uris.lv2_Plugin,
			dmanifest);
		SordIter* p = sord_begin(plugins);
		FOREACH_MATCH(p) {
			const SordNode* plug = sord_iter_get_node(p, SORD_SUBJECT);
			lilv_world_add_plugin(world, plug, manifest, desc, bundle_node);
		}
		if (desc->refs == 0) {
			lilv_dynmanifest_free(desc);
		}
		sord_iter_free(p);
		sord_free(plugins);
		lilv_free(lib_path);
	}
	sord_iter_free(iter);
	sord_free(model);
#endif  // LILV_DYN_MANIFEST
}

#ifdef LILV_DYN_MANIFEST
void
lilv_dynmanifest_free(LilvDynManifest* dynmanifest)
{
	typedef int (*CloseFunc)(LV2_Dyn_Manifest_Handle);
	CloseFunc close_func = (CloseFunc)lilv_dlfunc(dynmanifest->lib,
		"lv2_dyn_manifest_close");
	if (close_func) {
		close_func(dynmanifest->handle);
	}

	dlclose(dynmanifest->lib);
	lilv_node_free(dynmanifest->bundle);
	free(dynmanifest);
}
#endif  // LILV_DYN_MANIFEST

LilvNode*
lilv_world_get_manifest_uri(LilvWorld* world, const LilvNode* bundle_uri)
{
	SerdNode manifest_uri = lilv_new_uri_relative_to_base(
		(const uint8_t*)"manifest.ttl",
		sord_node_get_string(bundle_uri->node));
	LilvNode* manifest = lilv_new_uri(world, (const char*)manifest_uri.buf);
	serd_node_free(&manifest_uri);
	return manifest;
}

static SordModel*
load_plugin_model(LilvWorld* world,
	const LilvNode* bundle_uri,
	const LilvNode* plugin_uri)
{
	// Create model and reader for loading into it
	SordNode* bundle_node = bundle_uri->node;
	SordModel* model = sord_new(world->world, SORD_SPO | SORD_OPS, false);
	SerdEnv* env = serd_env_new(sord_node_to_serd_node(bundle_node));
	SerdReader* reader = sord_new_reader(model, env, SERD_TURTLE, NULL);

	// Load manifest
	LilvNode* manifest_uri = lilv_world_get_manifest_uri(world, bundle_uri);
	serd_reader_add_blank_prefix(reader, lilv_world_blank_node_prefix(world));
	serd_reader_read_file(
		reader, (const uint8_t*)lilv_node_as_string(manifest_uri));

	// Load any seeAlso files
	SordModel* files = lilv_world_filter_model(
		world, model, plugin_uri->node, world->uris.rdfs_seeAlso, NULL, NULL);

	SordIter* f = sord_begin(files);
	FOREACH_MATCH(f) {
		const SordNode* file = sord_iter_get_node(f, SORD_OBJECT);
		const uint8_t* file_str = sord_node_get_string(file);
		if (sord_node_get_type(file) == SORD_URI) {
			serd_reader_add_blank_prefix(
				reader, lilv_world_blank_node_prefix(world));
			serd_reader_read_file(reader, file_str);
		}
	}

	sord_iter_free(f);
	sord_free(files);
	serd_reader_free(reader);
	serd_env_free(env);
	lilv_node_free(manifest_uri);

	return model;
}

static LilvVersion
get_version(LilvWorld* world, SordModel* model, const LilvNode* subject)
{
	const SordNode* minor_node = sord_get(
		model, subject->node, world->uris.lv2_minorVersion, NULL, NULL);
	const SordNode* micro_node = sord_get(
		model, subject->node, world->uris.lv2_microVersion, NULL, NULL);


	LilvVersion version = { 0, 0 };
	if (minor_node && micro_node) {
		version.minor = atoi((const char*)sord_node_get_string(minor_node));
		version.micro = atoi((const char*)sord_node_get_string(micro_node));
	}

	return version;
}

void
lilv_world_load_bundle(LilvWorld* world, const LilvNode* bundle_uri)
{
	if (!lilv_node_is_uri(bundle_uri)) {
		LILV_ERRORF("Bundle URI `%s' is not a URI\n",
			sord_node_get_string(bundle_uri->node));
		return;
	}

	SordNode* bundle_node = bundle_uri->node;
	LilvNode* manifest = lilv_world_get_manifest_uri(world, bundle_uri);

	// Read manifest into model with graph = bundle_node
	SerdStatus st = lilv_world_load_graph(world, bundle_node, manifest);
	if (st > SERD_FAILURE) {
		LILV_ERRORF("Error reading %s\n", lilv_node_as_string(manifest));
		lilv_node_free(manifest);
		return;
	}

	// ?plugin a lv2:Plugin
	SordIter* plug_results = sord_search(world->model,
		NULL,
		world->uris.rdf_a,
		world->uris.lv2_Plugin,
		bundle_node);

	// Find any loaded plugins that will be replaced with a newer version
	LilvNodes* unload_uris = lilv_nodes_new();
	FOREACH_MATCH(plug_results) {
		const SordNode* plug = sord_iter_get_node(plug_results, SORD_SUBJECT);

		LilvNode* plugin_uri = lilv_node_new_from_node(world, plug);
		const LilvPlugin* plugin = lilv_plugins_get_by_uri(world->plugins, plugin_uri);
		const LilvNode* last_bundle = plugin ? lilv_plugin_get_bundle_uri(plugin) : NULL;
		if (!plugin || sord_node_equals(bundle_node, last_bundle->node)) {
			// No previously loaded version, or it's from the same bundle
			lilv_node_free(plugin_uri);
			continue;
		}

		// Compare versions
		SordModel* this_model = load_plugin_model(world, bundle_uri, plugin_uri);
		LilvVersion this_version = get_version(world, this_model, plugin_uri);
		SordModel* last_model = load_plugin_model(world, last_bundle, plugin_uri);
		LilvVersion last_version = get_version(world, last_model, plugin_uri);
		sord_free(this_model);
		sord_free(last_model);
		const int cmp = lilv_version_cmp(&this_version, &last_version);
		if (cmp > 0) {
			zix_tree_insert((ZixTree*)unload_uris,
				lilv_node_duplicate(plugin_uri),
				NULL);
			LILV_WARNF("Replacing version %d.%d of <%s> from <%s>\n",
				last_version.minor, last_version.micro,
				sord_node_get_string(plug),
				sord_node_get_string(last_bundle->node));
			LILV_NOTEF("New version %d.%d found in <%s>\n",
				this_version.minor, this_version.micro,
				sord_node_get_string(bundle_node));
		}
		else if (cmp < 0) {
			LILV_WARNF("Ignoring bundle <%s>\n",
				sord_node_get_string(bundle_node));
			LILV_NOTEF("Newer version of <%s> loaded from <%s>\n",
				sord_node_get_string(plug),
				sord_node_get_string(last_bundle->node));
			lilv_node_free(plugin_uri);
			sord_iter_free(plug_results);
			lilv_world_drop_graph(world, bundle_node);
			lilv_node_free(manifest);
			lilv_nodes_free(unload_uris);
			return;
		}
		lilv_node_free(plugin_uri);
	}

	sord_iter_free(plug_results);

	// Unload any old conflicting plugins
	LilvNodes* unload_bundles = lilv_nodes_new();
	LILV_FOREACH(nodes, i, unload_uris) {
		const LilvNode* uri = lilv_nodes_get(unload_uris, i);
		const LilvPlugin* plugin = lilv_plugins_get_by_uri(world->plugins, uri);
		const LilvNode* bundle = lilv_plugin_get_bundle_uri(plugin);

		// Unload plugin and record bundle for later unloading
		lilv_world_unload_resource(world, uri);
		zix_tree_insert((ZixTree*)unload_bundles,
			lilv_node_duplicate(bundle),
			NULL);

	}
	lilv_nodes_free(unload_uris);

	// Now unload the associated bundles
	// This must be done last since several plugins could be in the same bundle
	LILV_FOREACH(nodes, i, unload_bundles) {
		lilv_world_unload_bundle(world, lilv_nodes_get(unload_bundles, i));
	}
	lilv_nodes_free(unload_bundles);

	// Re-search for plugin results now that old plugins are gone
	plug_results = sord_search(world->model,
		NULL,
		world->uris.rdf_a,
		world->uris.lv2_Plugin,
		bundle_node);

	FOREACH_MATCH(plug_results) {
		const SordNode* plug = sord_iter_get_node(plug_results, SORD_SUBJECT);
		lilv_world_add_plugin(world, plug, manifest, NULL, bundle_node);
	}
	sord_iter_free(plug_results);

	lilv_world_load_dyn_manifest(world, bundle_node, manifest);

	// ?spec a lv2:Specification
	// ?spec a owl:Ontology
	const SordNode* spec_preds[] = { world->uris.lv2_Specification,
									 world->uris.owl_Ontology,
									 NULL };
	for (const SordNode** p = spec_preds; *p; ++p) {
		SordIter* i = sord_search(
			world->model, NULL, world->uris.rdf_a, *p, bundle_node);
		FOREACH_MATCH(i) {
			const SordNode* spec = sord_iter_get_node(i, SORD_SUBJECT);
			lilv_world_add_spec(world, spec, bundle_node);
		}
		sord_iter_free(i);
	}

	lilv_node_free(manifest);
}

static int
lilv_world_drop_graph(LilvWorld* world, const SordNode* graph)
{
	SordIter* i = sord_search(world->model, NULL, NULL, NULL, graph);
	while (!sord_iter_end(i)) {
		const SerdStatus st = sord_erase(world->model, i);
		if (st) {
			LILV_ERRORF("Error removing statement from <%s> (%s)\n",
				sord_node_get_string(graph), serd_strerror(st));
			return st;
		}
	}
	sord_iter_free(i);

	return 0;
}

/** Remove loaded_files entry so file will be reloaded if requested. */
static int
lilv_world_unload_file(LilvWorld* world, const LilvNode* file)
{
	ZixTreeIter* iter;
	if (!zix_tree_find((ZixTree*)world->loaded_files, file, &iter)) {
		zix_tree_remove((ZixTree*)world->loaded_files, iter);
		return 0;
	}
	return 1;
}

int
lilv_world_unload_bundle(LilvWorld* world, const LilvNode* bundle_uri)
{
	if (!bundle_uri) {
		return 0;
	}

	// Find all loaded files that are inside the bundle
	LilvNodes* files = lilv_nodes_new();
	LILV_FOREACH(nodes, i, world->loaded_files) {
		const LilvNode* file = lilv_nodes_get(world->loaded_files, i);
		if (!strncmp(lilv_node_as_string(file),
			lilv_node_as_string(bundle_uri),
			strlen(lilv_node_as_string(bundle_uri)))) {
			zix_tree_insert((ZixTree*)files,
				lilv_node_duplicate(file),
				NULL);
		}
	}

	// Unload all loaded files in the bundle
	LILV_FOREACH(nodes, i, files) {
		const LilvNode* file = lilv_nodes_get(world->plugins, i);
		lilv_world_unload_file(world, file);
	}

	lilv_nodes_free(files);

	/* Remove any plugins in the bundle from the plugin list.  Since the
	   application may still have a pointer to the LilvPlugin, it can not be
	   destroyed here.  Instead, we move it to the zombie plugin list, so it
	   will not be in the list returned by lilv_world_get_all_plugins() but can
	   still be used.
	*/
	ZixTreeIter* i = zix_tree_begin((ZixTree*)world->plugins);
	while (i != zix_tree_end((ZixTree*)world->plugins)) {
		LilvPlugin* p = (LilvPlugin*)zix_tree_get(i);
		ZixTreeIter* next = zix_tree_iter_next(i);

		if (lilv_node_equals(lilv_plugin_get_bundle_uri(p), bundle_uri)) {
			zix_tree_remove((ZixTree*)world->plugins, i);
			zix_tree_insert((ZixTree*)world->zombies, p, NULL);
		}

		i = next;
	}

	// Drop everything in bundle graph
	return lilv_world_drop_graph(world, bundle_uri->node);
}

static void
load_dir_entry(const char* dir, const char* name, void* data)
{
	LilvWorld* world = (LilvWorld*)data;
	if (!strcmp(name, ".") || !strcmp(name, "..")) {
		return;
	}

	char* path = lilv_strjoin(dir, "/", name, "/", NULL);
	SerdNode  suri = serd_node_new_file_uri((const uint8_t*)path, 0, 0, true);
	LilvNode* node = lilv_new_uri(world, (const char*)suri.buf);

	lilv_world_load_bundle(world, node);
	lilv_node_free(node);
	serd_node_free(&suri);
	free(path);
}

/** Load all bundles in the directory at `dir_path`. */
static void
lilv_world_load_directory(LilvWorld* world, const char* dir_path)
{
	char* path = lilv_expand(dir_path);
	if (path) {
		lilv_dir_for_each(path, world, load_dir_entry);
		free(path);
	}
}

static const char*
first_path_sep(const char* path)
{
	for (const char* p = path; *p != '\0'; ++p) {
		if (*p == LILV_PATH_SEP[0]) {
			return p;
		}
	}
	return NULL;
}

/** Load all bundles found in `lv2_path`.
 * @param lv2_path A colon-delimited list of directories.  These directories
 * should contain LV2 bundle directories (ie the search path is a list of
 * parent directories of bundles, not a list of bundle directories).
 */
static void
lilv_world_load_path(LilvWorld* world,
	const char* lv2_path)
{
	while (lv2_path[0] != '\0') {
		const char* const sep = first_path_sep(lv2_path);
		if (sep) {
			const size_t dir_len = sep - lv2_path;
			char* const  dir = (char*)malloc(dir_len + 1);
			memcpy(dir, lv2_path, dir_len);
			dir[dir_len] = '\0';
			lilv_world_load_directory(world, dir);
			free(dir);
			lv2_path += dir_len + 1;
		}
		else {
			lilv_world_load_directory(world, lv2_path);
			lv2_path = "\0";
		}
	}
}

void
lilv_world_load_specifications(LilvWorld* world)
{
	for (LilvSpec* spec = world->specs; spec; spec = spec->next) {
		LILV_FOREACH(nodes, f, spec->data_uris) {
			LilvNode* file = (LilvNode*)lilv_collection_get(spec->data_uris, f);
			lilv_world_load_graph(world, NULL, file);
		}
	}
}

void
lilv_world_load_plugin_classes(LilvWorld* world)
{
	/* FIXME: This loads all classes, not just lv2:Plugin subclasses.
	   However, if the host gets all the classes via lilv_plugin_class_get_children
	   starting with lv2:Plugin as the root (which is e.g. how a host would build
	   a menu), they won't be seen anyway...
	*/

	SordIter* classes = sord_search(world->model,
		NULL,
		world->uris.rdf_a,
		world->uris.rdfs_Class,
		NULL);
	FOREACH_MATCH(classes) {
		const SordNode* class_node = sord_iter_get_node(classes, SORD_SUBJECT);

		SordNode* parent = sord_get(
			world->model, class_node, world->uris.rdfs_subClassOf, NULL, NULL);
		if (!parent || sord_node_get_type(parent) != SORD_URI) {
			continue;
		}

		SordNode* label = sord_get(
			world->model, class_node, world->uris.rdfs_label, NULL, NULL);
		if (!label) {
			sord_node_free(world->world, parent);
			continue;
		}

		LilvPluginClass* pclass = lilv_plugin_class_new(
			world, parent, class_node,
			(const char*)sord_node_get_string(label));
		if (pclass) {
			zix_tree_insert((ZixTree*)world->plugin_classes, pclass, NULL);
		}

		sord_node_free(world->world, label);
		sord_node_free(world->world, parent);
	}
	sord_iter_free(classes);
}

void
lilv_world_load_all(LilvWorld* world)
{
	const char* lv2_path = world->opt.lv2_path;
	if (!lv2_path) {
		lv2_path = getenv("LV2_PATH");
	}
	if (!lv2_path) {
		lv2_path = LILV_DEFAULT_LV2_PATH;
	}

	// Discover bundles and read all manifest files into model
	lilv_world_load_path(world, lv2_path);

	LILV_FOREACH(plugins, p, world->plugins) {
		const LilvPlugin* plugin = (const LilvPlugin*)lilv_collection_get(
			(ZixTree*)world->plugins, p);

		// ?new dc:replaces plugin
		if (sord_ask(world->model,
			NULL,
			world->uris.dc_replaces,
			lilv_plugin_get_uri(plugin)->node,
			NULL)) {
			// TODO: Check if replacement is a known plugin? (expensive)
			((LilvPlugin*)plugin)->replaced = true;
		}
	}

	// Query out things to cache
	lilv_world_load_specifications(world);
	lilv_world_load_plugin_classes(world);
}

SerdStatus
lilv_world_load_file(LilvWorld* world, SerdReader* reader, const LilvNode* uri)
{
	ZixTreeIter* iter;
	if (!zix_tree_find((ZixTree*)world->loaded_files, uri, &iter)) {
		return SERD_FAILURE;  // File has already been loaded
	}

	size_t               uri_len;
	const uint8_t* const uri_str = sord_node_get_string_counted(
		uri->node, &uri_len);
	if (strncmp((const char*)uri_str, "file:", 5)) {
		return SERD_FAILURE;  // Not a local file
	}
	else if (strcmp((const char*)uri_str + uri_len - 4, ".ttl")) {
		return SERD_FAILURE;  // Not a Turtle file
	}

	serd_reader_add_blank_prefix(reader, lilv_world_blank_node_prefix(world));
	const SerdStatus st = serd_reader_read_file(reader, uri_str);
	if (st) {
		LILV_ERRORF("Error loading file `%s'\n", lilv_node_as_string(uri));
		return st;
	}

	zix_tree_insert((ZixTree*)world->loaded_files,
		lilv_node_duplicate(uri),
		NULL);
	return SERD_SUCCESS;
}

int
lilv_world_load_resource(LilvWorld* world,
	const LilvNode* resource)
{
	if (!lilv_node_is_uri(resource) && !lilv_node_is_blank(resource)) {
		LILV_ERRORF("Node `%s' is not a resource\n",
			sord_node_get_string(resource->node));
		return -1;
	}

	SordModel* files = lilv_world_filter_model(world,
		world->model,
		resource->node,
		world->uris.rdfs_seeAlso,
		NULL, NULL);

	SordIter* f = sord_begin(files);
	int       n_read = 0;
	FOREACH_MATCH(f) {
		const SordNode* file = sord_iter_get_node(f, SORD_OBJECT);
		const uint8_t* file_str = sord_node_get_string(file);
		LilvNode* file_node = lilv_node_new_from_node(world, file);
		if (sord_node_get_type(file) != SORD_URI) {
			LILV_ERRORF("rdfs:seeAlso node `%s' is not a URI\n", file_str);
		}
		else if (!lilv_world_load_graph(world, (SordNode*)file, file_node)) {
			++n_read;
		}
		lilv_node_free(file_node);
	}
	sord_iter_free(f);

	sord_free(files);
	return n_read;
}

int
lilv_world_unload_resource(LilvWorld* world,
	const LilvNode* resource)
{
	if (!lilv_node_is_uri(resource) && !lilv_node_is_blank(resource)) {
		LILV_ERRORF("Node `%s' is not a resource\n",
			sord_node_get_string(resource->node));
		return -1;
	}

	SordModel* files = lilv_world_filter_model(world,
		world->model,
		resource->node,
		world->uris.rdfs_seeAlso,
		NULL, NULL);

	SordIter* f = sord_begin(files);
	int       n_dropped = 0;
	FOREACH_MATCH(f) {
		const SordNode* file = sord_iter_get_node(f, SORD_OBJECT);
		LilvNode* file_node = lilv_node_new_from_node(world, file);
		if (sord_node_get_type(file) != SORD_URI) {
			LILV_ERRORF("rdfs:seeAlso node `%s' is not a URI\n",
				sord_node_get_string(file));
		}
		else if (!lilv_world_drop_graph(world, file_node->node)) {
			lilv_world_unload_file(world, file_node);
			++n_dropped;
		}
		lilv_node_free(file_node);
	}
	sord_iter_free(f);

	sord_free(files);
	return n_dropped;
}

const LilvPluginClass*
lilv_world_get_plugin_class(const LilvWorld* world)
{
	return world->lv2_plugin_class;
}

const LilvPluginClasses*
lilv_world_get_plugin_classes(const LilvWorld* world)
{
	return world->plugin_classes;
}

const LilvPlugins*
lilv_world_get_all_plugins(const LilvWorld* world)
{
	return world->plugins;
}

LilvNode*
lilv_world_get_symbol(LilvWorld* world, const LilvNode* subject)
{
	// Check for explicitly given symbol
	SordNode* snode = sord_get(
		world->model, subject->node, world->uris.lv2_symbol, NULL, NULL);

	if (snode) {
		LilvNode* ret = lilv_node_new_from_node(world, snode);
		sord_node_free(world->world, snode);
		return ret;
	}

	if (!lilv_node_is_uri(subject)) {
		return NULL;
	}

	// Find rightmost segment of URI
	SerdURI uri;
	serd_uri_parse((const uint8_t*)lilv_node_as_uri(subject), &uri);
	const char* str = "_";
	if (uri.fragment.buf) {
		str = (const char*)uri.fragment.buf + 1;
	}
	else if (uri.query.buf) {
		str = (const char*)uri.query.buf;
	}
	else if (uri.path.buf) {
		const char* last_slash = strrchr((const char*)uri.path.buf, '/');
		str = last_slash ? (last_slash + 1) : (const char*)uri.path.buf;
	}

	// Replace invalid characters
	const size_t len = strlen(str);
	char* const  sym = (char*)calloc(1, len + 1);
	for (size_t i = 0; i < len; ++i) {
		const char c = str[i];
		if (!((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
			(c == '_') || (i > 0 && c >= '0' && c <= '9'))) {
			sym[i] = '_';
		}
		else {
			sym[i] = str[i];
		}
	}

	LilvNode* ret = lilv_new_string(world, sym);
	free(sym);
	return ret;
}


typedef struct ZixTreeNodeImpl ZixTreeNode;

struct ZixTreeImpl {
	ZixTreeNode* root;
	ZixDestroyFunc destroy;
	ZixComparator  cmp;
	void* cmp_data;
	size_t         size;
	bool           allow_duplicates;
};

struct ZixTreeNodeImpl {
	void* data;
	struct ZixTreeNodeImpl* left;
	struct ZixTreeNodeImpl* right;
	struct ZixTreeNodeImpl* parent;
	int_fast8_t             balance;
};

#define MIN(a, b) (((a) < (b)) ? (a) : (b))
#define MAX(a, b) (((a) > (b)) ? (a) : (b))

// Uncomment these for debugging features
// #define ZIX_TREE_DUMP         1
// #define ZIX_TREE_VERIFY       1
// #define ZIX_TREE_HYPER_VERIFY 1

#if defined(ZIX_TREE_VERIFY) || defined(ZIX_TREE_HYPER_VERIFY)
#    include "tree_debug.h"
#    define ASSERT_BALANCE(n) assert(verify_balance(n))
#else
#    define ASSERT_BALANCE(n)
#endif

#ifdef ZIX_TREE_DUMP
#    include "tree_debug.h"
#    define DUMP(t) zix_tree_print(t->root, 0)
#    define DEBUG_PRINTF(fmt, ...) printf(fmt, __VA_ARGS__)
#else
#    define DUMP(t)
#    define DEBUG_PRINTF(fmt, ...)
#endif

ZIX_API ZixTree*
zix_tree_new(bool           allow_duplicates,
	ZixComparator  cmp,
	void* cmp_data,
	ZixDestroyFunc destroy)
{
	ZixTree* t = (ZixTree*)malloc(sizeof(ZixTree));
	t->root = NULL;
	t->destroy = destroy;
	t->cmp = cmp;
	t->cmp_data = cmp_data;
	t->size = 0;
	t->allow_duplicates = allow_duplicates;
	return t;
}

ZIX_PRIVATE void
zix_tree_free_rec(ZixTree* t, ZixTreeNode* n)
{
	if (n) {
		zix_tree_free_rec(t, n->left);
		zix_tree_free_rec(t, n->right);
		if (t->destroy) {
			t->destroy(n->data);
		}
		free(n);
	}
}

ZIX_API void
zix_tree_free(ZixTree* t)
{
	if (t) {
		zix_tree_free_rec(t, t->root);
		free(t);
	}
}

ZIX_API size_t
zix_tree_size(const ZixTree* t)
{
	return t->size;
}

ZIX_PRIVATE void
rotate(ZixTreeNode* p, ZixTreeNode* q)
{
	assert(q->parent == p);
	assert(p->left == q || p->right == q);

	q->parent = p->parent;
	if (q->parent) {
		if (q->parent->left == p) {
			q->parent->left = q;
		}
		else {
			q->parent->right = q;
		}
	}

	if (p->right == q) {
		// Rotate left
		p->right = q->left;
		q->left = p;
		if (p->right) {
			p->right->parent = p;
		}
	}
	else {
		// Rotate right
		assert(p->left == q);
		p->left = q->right;
		q->right = p;
		if (p->left) {
			p->left->parent = p;
		}
	}

	p->parent = q;
}

/**
 * Rotate left about `p`.
 *
 *    p              q
 *   / \            / \
 *  A   q    =>    p   C
 *     / \        / \
 *    B   C      A   B
 */
ZIX_PRIVATE ZixTreeNode*
rotate_left(ZixTreeNode* p, int* height_change)
{
	ZixTreeNode* const q = p->right;
	*height_change = (q->balance == 0) ? 0 : -1;

	DEBUG_PRINTF("LL %ld\n", (intptr_t)p->data);

	assert(p->balance == 2);
	assert(q->balance == 0 || q->balance == 1);

	rotate(p, q);

	// p->balance -= 1 + MAX(0, q->balance);
	// q->balance -= 1 - MIN(0, p->balance);
	--q->balance;
	p->balance = -(q->balance);

	ASSERT_BALANCE(p);
	ASSERT_BALANCE(q);
	return q;
}

/**
 * Rotate right about `p`.
 *
 *      p          q
 *     / \        / \
 *    q   C  =>  A   p
 *   / \            / \
 *  A   B          B   C
 *
 */
ZIX_PRIVATE ZixTreeNode*
rotate_right(ZixTreeNode* p, int* height_change)
{
	ZixTreeNode* const q = p->left;
	*height_change = (q->balance == 0) ? 0 : -1;

	DEBUG_PRINTF("RR %ld\n", (intptr_t)p->data);

	assert(p->balance == -2);
	assert(q->balance == 0 || q->balance == -1);

	rotate(p, q);

	// p->balance += 1 - MIN(0, q->balance);
	// q->balance += 1 + MAX(0, p->balance);
	++q->balance;
	p->balance = -(q->balance);

	ASSERT_BALANCE(p);
	ASSERT_BALANCE(q);
	return q;
}

/**
 * Rotate left about `p->left` then right about `p`.
 *
 *      p             r
 *     / \           / \
 *    q   D  =>    q     p
 *   / \          / \   / \
 *  A   r        A   B C   D
 *     / \
 *    B   C
 *
 */
ZIX_PRIVATE ZixTreeNode*
rotate_left_right(ZixTreeNode* p, int* height_change)
{
	ZixTreeNode* const q = p->left;
	ZixTreeNode* const r = q->right;

	assert(p->balance == -2);
	assert(q->balance == 1);
	assert(r->balance == -1 || r->balance == 0 || r->balance == 1);

	DEBUG_PRINTF("LR %ld  P: %2d  Q: %2d  R: %2d\n",
		(intptr_t)p->data, p->balance, q->balance, r->balance);

	rotate(q, r);
	rotate(p, r);

	q->balance -= 1 + MAX(0, r->balance);
	p->balance += 1 - MIN(MIN(0, r->balance) - 1, r->balance + q->balance);
	// r->balance += MAX(0, p->balance) + MIN(0, q->balance);

	// p->balance = (p->left && p->right) ? -MIN(r->balance, 0) : 0;
	// q->balance = - MAX(r->balance, 0);
	r->balance = 0;

	*height_change = -1;

	ASSERT_BALANCE(p);
	ASSERT_BALANCE(q);
	ASSERT_BALANCE(r);
	return r;
}

/**
 * Rotate right about `p->right` then right about `p`.
 *
 *    p               r
 *   / \             / \
 *  A   q    =>    p     q
 *     / \        / \   / \
 *    r   D      A   B C   D
 *   / \
 *  B   C
 *
 */
ZIX_PRIVATE ZixTreeNode*
rotate_right_left(ZixTreeNode* p, int* height_change)
{
	ZixTreeNode* const q = p->right;
	ZixTreeNode* const r = q->left;

	assert(p->balance == 2);
	assert(q->balance == -1);
	assert(r->balance == -1 || r->balance == 0 || r->balance == 1);

	DEBUG_PRINTF("RL %ld  P: %2d  Q: %2d  R: %2d\n",
		(intptr_t)p->data, p->balance, q->balance, r->balance);

	rotate(q, r);
	rotate(p, r);

	q->balance += 1 - MIN(0, r->balance);
	p->balance -= 1 + MAX(MAX(0, r->balance) + 1, r->balance + q->balance);
	// r->balance += MAX(0, q->balance) + MIN(0, p->balance);

	// p->balance = (p->left && p->right) ? -MAX(r->balance, 0) : 0;
	// q->balance = - MIN(r->balance, 0);
	r->balance = 0;
	// assert(r->balance == 0);

	*height_change = -1;

	ASSERT_BALANCE(p);
	ASSERT_BALANCE(q);
	ASSERT_BALANCE(r);
	return r;
}

ZIX_PRIVATE ZixTreeNode*
zix_tree_rebalance(ZixTree* t, ZixTreeNode* node, int* height_change)
{
#ifdef ZIX_TREE_HYPER_VERIFY
	const size_t old_height = height(node);
#endif
	DEBUG_PRINTF("REBALANCE %ld (%d)\n", (intptr_t)node->data, node->balance);
	*height_change = 0;
	const bool is_root = !node->parent;
	assert((is_root && t->root == node) || (!is_root && t->root != node));
	ZixTreeNode* replacement = node;
	if (node->balance == -2) {
		assert(node->left);
		if (node->left->balance == 1) {
			replacement = rotate_left_right(node, height_change);
		}
		else {
			replacement = rotate_right(node, height_change);
		}
	}
	else if (node->balance == 2) {
		assert(node->right);
		if (node->right->balance == -1) {
			replacement = rotate_right_left(node, height_change);
		}
		else {
			replacement = rotate_left(node, height_change);
		}
	}
	if (is_root) {
		assert(!replacement->parent);
		t->root = replacement;
	}
	DUMP(t);
#ifdef ZIX_TREE_HYPER_VERIFY
	assert(old_height + *height_change == height(replacement));
#endif
	return replacement;
}

ZIX_API ZixStatus
zix_tree_insert(ZixTree* t, void* e, ZixTreeIter** ti)
{
	DEBUG_PRINTF("**** INSERT %ld\n", (intptr_t)e);
	int          cmp = 0;
	ZixTreeNode* n = t->root;
	ZixTreeNode* p = NULL;

	// Find the parent p of e
	while (n) {
		p = n;
		cmp = t->cmp(e, n->data, t->cmp_data);
		if (cmp < 0) {
			n = n->left;
		}
		else if (cmp > 0) {
			n = n->right;
		}
		else if (t->allow_duplicates) {
			n = n->right;
		}
		else {
			if (ti) {
				*ti = n;
			}
			DEBUG_PRINTF("%ld EXISTS!\n", (intptr_t)e);
			return ZIX_STATUS_EXISTS;
		}
	}

	// Allocate a new node n
	if (!(n = (ZixTreeNode*)malloc(sizeof(ZixTreeNode)))) {
		return ZIX_STATUS_NO_MEM;
	}
	memset(n, '\0', sizeof(ZixTreeNode));
	n->data = e;
	n->balance = 0;
	if (ti) {
		*ti = n;
	}

	bool p_height_increased = false;

	// Make p the parent of n
	n->parent = p;
	if (!p) {
		t->root = n;
	}
	else {
		if (cmp < 0) {
			assert(!p->left);
			assert(p->balance == 0 || p->balance == 1);
			p->left = n;
			--p->balance;
			p_height_increased = !p->right;
		}
		else {
			assert(!p->right);
			assert(p->balance == 0 || p->balance == -1);
			p->right = n;
			++p->balance;
			p_height_increased = !p->left;
		}
	}

	DUMP(t);

	// Rebalance if necessary (at most 1 rotation)
	assert(!p || p->balance == -1 || p->balance == 0 || p->balance == 1);
	if (p && p_height_increased) {
		int height_change = 0;
		for (ZixTreeNode* i = p; i && i->parent; i = i->parent) {
			if (i == i->parent->left) {
				if (--i->parent->balance == -2) {
					zix_tree_rebalance(t, i->parent, &height_change);
					break;
				}
			}
			else {
				assert(i == i->parent->right);
				if (++i->parent->balance == 2) {
					zix_tree_rebalance(t, i->parent, &height_change);
					break;
				}
			}

			if (i->parent->balance == 0) {
				break;
			}
		}
	}

	DUMP(t);

	++t->size;

#ifdef ZIX_TREE_VERIFY
	if (!verify(t, t->root)) {
		return ZIX_STATUS_ERROR;
	}
#endif

	return ZIX_STATUS_SUCCESS;
}

ZIX_API ZixStatus
zix_tree_remove(ZixTree* t, ZixTreeIter* ti)
{
	ZixTreeNode* const n = ti;
	ZixTreeNode** pp = NULL;  // parent pointer
	ZixTreeNode* to_balance = n->parent;  // lowest node to balance
	int8_t             d_balance = 0;  // delta(balance) for n->parent

	DEBUG_PRINTF("*** REMOVE %ld\n", (intptr_t)n->data);

	if ((n == t->root) && !n->left && !n->right) {
		t->root = NULL;
		if (t->destroy) {
			t->destroy(n->data);
		}
		free(n);
		--t->size;
		assert(t->size == 0);
		return ZIX_STATUS_SUCCESS;
	}

	// Set pp to the parent pointer to n, if applicable
	if (n->parent) {
		assert(n->parent->left == n || n->parent->right == n);
		if (n->parent->left == n) {  // n is left child
			pp = &n->parent->left;
			d_balance = 1;
		}
		else {  // n is right child
			assert(n->parent->right == n);
			pp = &n->parent->right;
			d_balance = -1;
		}
	}

	assert(!pp || *pp == n);

	int height_change = 0;
	if (!n->left && !n->right) {
		// n is a leaf, just remove it
		if (pp) {
			*pp = NULL;
			to_balance = n->parent;
			height_change = (!n->parent->left && !n->parent->right) ? -1 : 0;
		}
	}
	else if (!n->left) {
		// Replace n with right (only) child
		if (pp) {
			*pp = n->right;
			to_balance = n->parent;
		}
		else {
			t->root = n->right;
		}
		n->right->parent = n->parent;
		height_change = -1;
	}
	else if (!n->right) {
		// Replace n with left (only) child
		if (pp) {
			*pp = n->left;
			to_balance = n->parent;
		}
		else {
			t->root = n->left;
		}
		n->left->parent = n->parent;
		height_change = -1;
	}
	else {
		// Replace n with in-order successor (leftmost child of right subtree)
		ZixTreeNode* replace = n->right;
		while (replace->left) {
			assert(replace->left->parent == replace);
			replace = replace->left;
		}

		// Remove replace from parent (replace_p)
		if (replace->parent->left == replace) {
			height_change = replace->parent->right ? 0 : -1;
			d_balance = 1;
			to_balance = replace->parent;
			replace->parent->left = replace->right;
		}
		else {
			assert(replace->parent == n);
			height_change = replace->parent->left ? 0 : -1;
			d_balance = -1;
			to_balance = replace->parent;
			replace->parent->right = replace->right;
		}

		if (to_balance == n) {
			to_balance = replace;
		}

		if (replace->right) {
			replace->right->parent = replace->parent;
		}

		replace->balance = n->balance;

		// Swap node to delete with replace
		if (pp) {
			*pp = replace;
		}
		else {
			assert(t->root == n);
			t->root = replace;
		}
		replace->parent = n->parent;
		replace->left = n->left;
		n->left->parent = replace;
		replace->right = n->right;
		if (n->right) {
			n->right->parent = replace;
		}

		assert(!replace->parent
			|| replace->parent->left == replace
			|| replace->parent->right == replace);
	}

	// Rebalance starting at to_balance upwards.
	for (ZixTreeNode* i = to_balance; i; i = i->parent) {
		i->balance += d_balance;
		if (d_balance == 0 || i->balance == -1 || i->balance == 1) {
			break;
		}

		assert(i != n);
		i = zix_tree_rebalance(t, i, &height_change);
		if (i->balance == 0) {
			height_change = -1;
		}

		if (i->parent) {
			if (i == i->parent->left) {
				d_balance = height_change * -1;
			}
			else {
				assert(i == i->parent->right);
				d_balance = height_change;
			}
		}
	}

	DUMP(t);

	if (t->destroy) {
		t->destroy(n->data);
	}
	free(n);

	--t->size;

#ifdef ZIX_TREE_VERIFY
	if (!verify(t, t->root)) {
		return ZIX_STATUS_ERROR;
	}
#endif

	return ZIX_STATUS_SUCCESS;
}

ZIX_API ZixStatus
zix_tree_find(const ZixTree* t, const void* e, ZixTreeIter** ti)
{
	ZixTreeNode* n = t->root;
	while (n) {
		const int cmp = t->cmp(e, n->data, t->cmp_data);
		if (cmp == 0) {
			break;
		}
		else if (cmp < 0) {
			n = n->left;
		}
		else {
			n = n->right;
		}
	}

	*ti = n;
	return (n) ? ZIX_STATUS_SUCCESS : ZIX_STATUS_NOT_FOUND;
}

ZIX_API void*
zix_tree_get(const ZixTreeIter* ti)
{
	return ti ? ti->data : NULL;
}

ZIX_API ZixTreeIter*
zix_tree_begin(ZixTree* t)
{
	if (!t->root) {
		return NULL;
	}

	ZixTreeNode* n = t->root;
	while (n->left) {
		n = n->left;
	}
	return n;
}

ZIX_API ZixTreeIter*
zix_tree_end(ZixTree* t)
{
	return NULL;
}

ZIX_API ZixTreeIter*
zix_tree_rbegin(ZixTree* t)
{
	if (!t->root) {
		return NULL;
	}

	ZixTreeNode* n = t->root;
	while (n->right) {
		n = n->right;
	}
	return n;
}

ZIX_API ZixTreeIter*
zix_tree_rend(ZixTree* t)
{
	return NULL;
}

ZIX_API bool
zix_tree_iter_is_end(const ZixTreeIter* i)
{
	return !i;
}

ZIX_API bool
zix_tree_iter_is_rend(const ZixTreeIter* i)
{
	return !i;
}

ZIX_API ZixTreeIter*
zix_tree_iter_next(ZixTreeIter* i)
{
	if (!i) {
		return NULL;
	}

	if (i->right) {
		i = i->right;
		while (i->left) {
			i = i->left;
		}
	}
	else {
		while (i->parent && i->parent->right == i) {  // i is a right child
			i = i->parent;
		}

		i = i->parent;
	}

	return i;
}

ZIX_API ZixTreeIter*
zix_tree_iter_prev(ZixTreeIter* i)
{
	if (!i) {
		return NULL;
	}

	if (i->left) {
		i = i->left;
		while (i->right) {
			i = i->right;
		}
	}
	else {
		while (i->parent && i->parent->left == i) {  // i is a left child
			i = i->parent;
		}

		i = i->parent;
	}

	return i;
}























#ifdef _WIN32
#ifndef _WIN32_WINNT
#    define _WIN32_WINNT 0x0600  /* for CreateSymbolicLink */
#endif
#    include <windows.h>
#    include <direct.h>
#    include <io.h>
#    define F_OK 0
#    define mkdir(path, flags) _mkdir(path)
#    if (defined(_MSC_VER) && _MSC_VER <= 1400) || defined(__MINGW64__) || defined(__MINGW32__)
/** Implement 'CreateSymbolicLink()' for MSVC 8 or earlier */
#ifdef __cplusplus
extern "C"
#endif
static BOOLEAN WINAPI
CreateSymbolicLink(LPCTSTR linkpath, LPCTSTR targetpath, DWORD flags)
{
	typedef BOOLEAN(WINAPI* PFUNC)(LPCTSTR, LPCTSTR, DWORD);

	PFUNC pfn = (PFUNC)GetProcAddress(GetModuleHandle(TEXT("kernel32.dll")),
		"CreateSymbolicLinkA");
	return pfn ? pfn(linkpath, targetpath, flags) : 0;
}
#    endif
#else
#    include <dirent.h>
#    include <unistd.h>
#endif

#if defined(HAVE_FLOCK) && defined(HAVE_FILENO)
#    include <sys/file.h>
#endif












#ifndef PAGE_SIZE
#    define PAGE_SIZE 4096
#endif

void
lilv_free(void* ptr)
{
	free(ptr);
}

char*
lilv_strjoin(const char* first, ...)
{
	size_t  len = strlen(first);
	char* result = (char*)malloc(len + 1);

	memcpy(result, first, len);

	va_list args;
	va_start(args, first);
	while (1) {
		const char* const s = va_arg(args, const char*);
		if (s == NULL) {
			break;
		}

		const size_t this_len = strlen(s);
		char* new_result = (char*)realloc(result, len + this_len + 1);
		if (!new_result) {
			free(result);
			return NULL;
		}

		result = new_result;
		memcpy(result + len, s, this_len);
		len += this_len;
	}
	va_end(args);

	result[len] = '\0';

	return result;
}

char*
lilv_strdup(const char* str)
{
	if (!str) {
		return NULL;
	}

	const size_t len = strlen(str);
	char* copy = (char*)malloc(len + 1);
	memcpy(copy, str, len + 1);
	return copy;
}

const char*
lilv_uri_to_path(const char* uri)
{
	return (const char*)serd_uri_to_path((const uint8_t*)uri);
}

char*
lilv_file_uri_parse(const char* uri, char** hostname)
{
	return (char*)serd_file_uri_parse((const uint8_t*)uri, (uint8_t**)hostname);
}

/** Return the current LANG converted to Turtle (i.e. RFC3066) style.
 * For example, if LANG is set to "en_CA.utf-8", this returns "en-ca".
 */
char*
lilv_get_lang(void)
{
	const char* const env_lang = getenv("LANG");
	if (!env_lang || !strcmp(env_lang, "")
		|| !strcmp(env_lang, "C") || !strcmp(env_lang, "POSIX")) {
		return NULL;
	}

	const size_t env_lang_len = strlen(env_lang);
	char* const  lang = (char*)malloc(env_lang_len + 1);
	for (size_t i = 0; i < env_lang_len + 1; ++i) {
		if (env_lang[i] == '_') {
			lang[i] = '-';  // Convert _ to -
		}
		else if (env_lang[i] >= 'A' && env_lang[i] <= 'Z') {
			lang[i] = env_lang[i] + ('a' - 'A');  // Convert to lowercase
		}
		else if (env_lang[i] >= 'a' && env_lang[i] <= 'z') {
			lang[i] = env_lang[i];  // Lowercase letter, copy verbatim
		}
		else if (env_lang[i] >= '0' && env_lang[i] <= '9') {
			lang[i] = env_lang[i];  // Digit, copy verbatim
		}
		else if (env_lang[i] == '\0' || env_lang[i] == '.') {
			// End, or start of suffix (e.g. en_CA.utf-8), finished
			lang[i] = '\0';
			break;
		}
		else {
			LILV_ERRORF("Illegal LANG `%s' ignored\n", env_lang);
			free(lang);
			return NULL;
		}
	}

	return lang;
}

#ifndef _WIN32

/** Append suffix to dst, update dst_len, and return the realloc'd result. */
static char*
strappend(char* dst, size_t* dst_len, const char* suffix, size_t suffix_len)
{
	dst = (char*)realloc(dst, *dst_len + suffix_len + 1);
	memcpy(dst + *dst_len, suffix, suffix_len);
	dst[(*dst_len += suffix_len)] = '\0';
	return dst;
}

/** Append the value of the environment variable var to dst. */
static char*
append_var(char* dst, size_t* dst_len, const char* var)
{
	// Get value from environment
	const char* val = getenv(var);
	if (val) {  // Value found, append it
		return strappend(dst, dst_len, val, strlen(val));
	}
	else {  // No value found, append variable reference as-is
		return strappend(strappend(dst, dst_len, "$", 1),
			dst_len, var, strlen(var));
	}
}

#endif

/** Expand variables (e.g. POSIX ~ or $FOO, Windows %FOO%) in `path`. */
char*
lilv_expand(const char* path)
{
#ifdef _WIN32
	char* out = (char*)malloc(MAX_PATH);
	ExpandEnvironmentStrings(path, out, MAX_PATH);
#else
	char* out = NULL;
	size_t len = 0;

	const char* start = path;  // Start of current chunk to copy
	for (const char* s = path; *s;) {
		if (*s == '$') {
			// Hit $ (variable reference, e.g. $VAR_NAME)
			for (const char* t = s + 1; ; ++t) {
				if (!*t || (!isupper(*t) && !isdigit(*t) && *t != '_')) {
					// Append preceding chunk
					out = strappend(out, &len, start, s - start);

					// Append variable value (or $VAR_NAME if not found)
					char* var = (char*)calloc(t - s, 1);
					memcpy(var, s + 1, t - s - 1);
					out = append_var(out, &len, var);
					free(var);

					// Continue after variable reference
					start = s = t;
					break;
				}
			}
		}
		else if (*s == '~' && (*(s + 1) == '/' || !*(s + 1))) {
			// Hit ~ before slash or end of string (home directory reference)
			out = strappend(out, &len, start, s - start);
			out = append_var(out, &len, "HOME");
			start = ++s;
		}
		else {
			++s;
		}
	}

	if (*start) {
		out = strappend(out, &len, start, strlen(start));
	}
#endif

	return out;
}

static bool
lilv_is_dir_sep(const char c)
{
	return c == '/' || c == LILV_DIR_SEP[0];
}

char*
lilv_dirname(const char* path)
{
	const char* s = path + strlen(path) - 1;  // Last character
	for (; s > path && lilv_is_dir_sep(*s); --s) {}  // Last non-slash
	for (; s > path && !lilv_is_dir_sep(*s); --s) {}  // Last internal slash
	for (; s > path && lilv_is_dir_sep(*s); --s) {}  // Skip duplicates

	if (s == path) {  // Hit beginning
		return lilv_is_dir_sep(*s) ? lilv_strdup("/") : lilv_strdup(".");
	}
	else {  // Pointing to the last character of the result (inclusive)
		char* dirname = (char*)malloc(s - path + 2);
		memcpy(dirname, path, s - path + 1);
		dirname[s - path + 1] = '\0';
		return dirname;
	}
}

char*
lilv_dir_path(const char* path)
{
	if (!path) {
		return NULL;
	}

	const size_t len = strlen(path);

	if (lilv_is_dir_sep(path[len - 1])) {
		return lilv_strdup(path);
	}

	char* dir_path = (char*)calloc(len + 2, 1);
	memcpy(dir_path, path, len);
	dir_path[len] = LILV_DIR_SEP[0];
	return dir_path;
}

bool
lilv_path_exists(const char* path, const void* ignored)
{
#ifdef HAVE_LSTAT
	struct stat st;
	return !lstat(path, &st);
#else
	return !access(path, F_OK);
#endif
}

char*
lilv_find_free_path(const char* in_path,
	bool (*exists)(const char*, const void*),
	const void* user_data)
{
	const size_t in_path_len = strlen(in_path);
	char* path = (char*)malloc(in_path_len + 7);
	memcpy(path, in_path, in_path_len + 1);

	for (int i = 2; i < 1000000; ++i) {
		if (!exists(path, user_data)) {
			return path;
		}
		snprintf(path, in_path_len + 7, "%s.%u", in_path, i);
	}

	return NULL;
}

int
lilv_copy_file(const char* src, const char* dst)
{
	FILE* in = fopen(src, "r");
	if (!in) {
		return errno;
	}

	FILE* out = fopen(dst, "w");
	if (!out) {
		fclose(in);
		return errno;
	}

	char* page = (char*)malloc(PAGE_SIZE);
	size_t n_read = 0;
	int    st = 0;
	while ((n_read = fread(page, 1, PAGE_SIZE, in)) > 0) {
		if (fwrite(page, 1, n_read, out) != n_read) {
			st = errno;
			break;
		}
	}

	if (!st && (ferror(in) || ferror(out))) {
		st = EBADF;
	}

	free(page);
	fclose(in);
	fclose(out);

	return st;
}

static inline bool
is_windows_path(const char* path)
{
	return (isalpha(path[0]) && (path[1] == ':' || path[1] == '|') &&
		(path[2] == '/' || path[2] == '\\'));
}

bool
lilv_path_is_absolute(const char* path)
{
	if (lilv_is_dir_sep(path[0])) {
		return true;
	}

#ifdef _WIN32
	if (is_windows_path(path)) {
		return true;
	}
#endif

	return false;
}

char*
lilv_path_absolute(const char* path)
{
	if (lilv_path_is_absolute(path)) {
		return lilv_strdup(path);
	}
	else {
		char* cwd = getcwd(NULL, 0);
		char* abs_path = lilv_path_join(cwd, path);
		free(cwd);
		return abs_path;
	}
}

char*
lilv_path_join(const char* a, const char* b)
{
	if (!a) {
		return lilv_strdup(b);
	}

	const size_t a_len = strlen(a);
	const size_t b_len = b ? strlen(b) : 0;
	const size_t pre_len = a_len - (lilv_is_dir_sep(a[a_len - 1]) ? 1 : 0);
	char* path = (char*)calloc(1, a_len + b_len + 2);
	memcpy(path, a, pre_len);
	path[pre_len] = '/';
	if (b) {
		memcpy(path + pre_len + 1,
			b + (lilv_is_dir_sep(b[0]) ? 1 : 0),
			lilv_is_dir_sep(b[0]) ? b_len - 1 : b_len);
	}
	return path;
}

typedef struct {
	char* pattern;
	time_t time;
	char* latest;
} Latest;

static void
update_latest(const char* path, const char* name, void* data)
{
	Latest* latest = (Latest*)data;
	char* entry_path = lilv_path_join(path, name);
	unsigned num;
	if (sscanf(entry_path, latest->pattern, &num) == 1) {
		struct stat st;
		if (!stat(entry_path, &st)) {
			if (st.st_mtime >= latest->time) {
				free(latest->latest);
				latest->latest = entry_path;
			}
		}
		else {
			LILV_ERRORF("stat(%s) (%s)\n", path, strerror(errno));
		}
	}
	if (entry_path != latest->latest) {
		free(entry_path);
	}
}

/** Return the latest copy of the file at `path` that is newer. */
char*
lilv_get_latest_copy(const char* path, const char* copy_path)
{
	char* copy_dir = lilv_dirname(copy_path);
	Latest latest = { lilv_strjoin(copy_path, ".%u", NULL), 0, NULL };

	struct stat st;
	if (!stat(path, &st)) {
		latest.time = st.st_mtime;
	}
	else {
		LILV_ERRORF("stat(%s) (%s)\n", path, strerror(errno));
	}

	lilv_dir_for_each(copy_dir, &latest, update_latest);

	free(latest.pattern);
	free(copy_dir);
	return latest.latest;
}

char*
lilv_realpath(const char* path)
{
	if (!path) {
		return NULL;
	}

#if defined(_WIN32)
	char* out = (char*)malloc(MAX_PATH);
	GetFullPathName(path, MAX_PATH, out, NULL);
	return out;
#else
	char* real_path = realpath(path, NULL);
	return real_path ? real_path : lilv_strdup(path);
#endif
}

int
lilv_symlink(const char* oldpath, const char* newpath)
{
	int ret = 0;
	if (strcmp(oldpath, newpath)) {
#ifdef _WIN32
		ret = !CreateSymbolicLink(newpath, oldpath, 0);
		if (ret) {
			ret = !CreateHardLink(newpath, oldpath, 0);
		}
#else
		ret = symlink(oldpath, newpath);
#endif
	}
	if (ret) {
		LILV_ERRORF("Failed to link %s => %s (%s)\n",
			newpath, oldpath, strerror(errno));
	}
	return ret;
}

char*
lilv_path_relative_to(const char* path, const char* base)
{
	const size_t path_len = strlen(path);
	const size_t base_len = strlen(base);
	const size_t min_len = (path_len < base_len) ? path_len : base_len;

	// Find the last separator common to both paths
	size_t last_shared_sep = 0;
	for (size_t i = 0; i < min_len && path[i] == base[i]; ++i) {
		if (lilv_is_dir_sep(path[i])) {
			last_shared_sep = i;
		}
	}

	if (last_shared_sep == 0) {
		// No common components, return path
		return lilv_strdup(path);
	}

	// Find the number of up references ("..") required
	size_t up = 0;
	for (size_t i = last_shared_sep + 1; i < base_len; ++i) {
		if (lilv_is_dir_sep(base[i])) {
			++up;
		}
	}

	// Write up references
	const size_t suffix_len = path_len - last_shared_sep;
	char* rel = (char*)calloc(1, suffix_len + (up * 3) + 1);
	for (size_t i = 0; i < up; ++i) {
		memcpy(rel + (i * 3), "../", 3);
	}

	// Write suffix
	memcpy(rel + (up * 3), path + last_shared_sep + 1, suffix_len);
	return rel;
}

bool
lilv_path_is_child(const char* path, const char* dir)
{
	if (path && dir) {
		const size_t path_len = strlen(path);
		const size_t dir_len = strlen(dir);
		return dir && path_len >= dir_len && !strncmp(path, dir, dir_len);
	}
	return false;
}

int
lilv_flock(FILE* file, bool lock)
{
#if defined(HAVE_FLOCK) && defined(HAVE_FILENO)
	return flock(fileno(file), lock ? LOCK_EX : LOCK_UN);
#else
	return 0;
#endif
}

void
lilv_dir_for_each(const char* path,
	void* data,
	void (*f)(const char* path, const char* name, void* data))
{
#ifdef _WIN32
	char* pat = lilv_path_join(path, "*");
	WIN32_FIND_DATA fd;
	HANDLE          fh = FindFirstFile(pat, &fd);
	if (fh != INVALID_HANDLE_VALUE) {
		do {
			f(path, fd.cFileName, data);
		} while (FindNextFile(fh, &fd));
	}
	free(pat);
#else
	DIR* dir = opendir(path);
	if (dir) {
		for (struct dirent* entry; (entry = readdir(dir));) {
			f(path, entry->d_name, data);
		}
		closedir(dir);
	}
#endif
}

int
lilv_mkdir_p(const char* dir_path)
{
	char* path = lilv_strdup(dir_path);
	const size_t path_len = strlen(path);
	size_t       i = 1;

#ifdef _WIN32
	if (is_windows_path(dir_path)) {
		i = 3;
	}
#endif

	for (; i <= path_len; ++i) {
		const char c = path[i];
		if (c == LILV_DIR_SEP[0] || c == '/' || c == '\0') {
			path[i] = '\0';
			if (mkdir(path, 0755) && errno != EEXIST) {
				free(path);
				return errno;
			}
			path[i] = c;
		}
	}

	free(path);
	return 0;
}

static off_t
lilv_file_size(const char* path)
{
	struct stat buf;
	if (stat(path, &buf)) {
		LILV_ERRORF("stat(%s) (%s)\n", path, strerror(errno));
		return 0;
	}
	return buf.st_size;
}

bool
lilv_file_equals(const char* a_path, const char* b_path)
{
	if (!strcmp(a_path, b_path)) {
		return true;  // Paths match
	}

	bool        match = false;
	FILE* a_file = NULL;
	FILE* b_file = NULL;
	char* const a_real = lilv_realpath(a_path);
	char* const b_real = lilv_realpath(b_path);
	if (!strcmp(a_real, b_real)) {
		match = true;  // Real paths match
	}
	else if (lilv_file_size(a_path) != lilv_file_size(b_path)) {
		match = false;  // Sizes differ
	}
	else if (!(a_file = fopen(a_real, "rb")) ||
		!(b_file = fopen(b_real, "rb"))) {
		match = false;  // Missing file matches nothing
	}
	else {
		// TODO: Improve performance by reading chunks
		match = true;
		while (!feof(a_file) && !feof(b_file)) {
			if (fgetc(a_file) != fgetc(b_file)) {
				match = false;
				break;
			}
		}
	}

	if (a_file) {
		fclose(a_file);
	}
	if (b_file) {
		fclose(b_file);
	}
	free(a_real);
	free(b_real);
	return match;
}
