#include <dlfcn.h>
#include <stdlib.h>
#include <iostream>
#include <sstream>
#include <stdexcept>
#include "ladspa_plugin.h"

using namespace std;

LadspaPlugin::LadspaPlugin (string const & filename, int index)
	: _descriptor (0)
	, _num_ports (0)
	, _handle (0)
	, _controls (0)
	, _buffers (0)
{
	_library = dlopen (filename.c_str (), RTLD_NOW);
	if (_library == 0) {
		stringstream s;
		s << "Could not dlopen() " << filename;
		throw runtime_error (s.str ());
	}
	
	LADSPA_Descriptor_Function fn = (LADSPA_Descriptor_Function) dlsym (_library, "ladspa_descriptor");
	if (fn == 0) {
		stringstream s;
		s << "Could not find ladspa_descriptor in " << filename;
		throw runtime_error (s.str ());
	}

	_descriptor = fn (index);
	_num_ports = _descriptor->PortCount;
}

LadspaPlugin::~LadspaPlugin ()
{
	unprepare ();
	dlclose (_library);
}

void
LadspaPlugin::instantiate (int Fs)
{
	Plugin::instantiate (Fs);
	
	_handle = _descriptor->instantiate (_descriptor, Fs);
	if (_handle == 0) {
		throw runtime_error ("Instantiation failed.");
	}
}

void
LadspaPlugin::activate ()
{
	if (_descriptor->activate) {
		_descriptor->activate (_handle);
	}
}

void
LadspaPlugin::deactivate ()
{
	if (_descriptor->deactivate) {
		_descriptor->deactivate (_handle);
	}
}

void
LadspaPlugin::unprepare ()
{
	free (_controls);

	for (int i = 0; i < _num_ports; ++i) {
		free (_buffers[i]);
	}
	
	free (_buffers);
}

void
LadspaPlugin::prepare (int buffer_size)
{
	LADSPA_PortRangeHint const * hints = _descriptor->PortRangeHints;
	_controls = (LADSPA_Data *) calloc (_num_ports, sizeof (LADSPA_Data));
	_buffers = (LADSPA_Data **) calloc (_num_ports, sizeof (LADSPA_Data *));

	for (int port = 0; port < _num_ports; ++port) {
		LADSPA_Data min, max, start;
    
		if (LADSPA_IS_HINT_SAMPLE_RATE (hints[port].HintDescriptor)) {
			min = hints[port].LowerBound * (float) _sampling_rate;
			max = hints[port].UpperBound * (float) _sampling_rate;
		} else {
			min = hints[port].LowerBound;
			max = hints[port].UpperBound;
		}
		
		_descriptor->connect_port(_handle, port, &(_controls[port]));
		if (LADSPA_IS_HINT_DEFAULT_MINIMUM (hints[port].HintDescriptor)) {
			start = min;
		} else if (LADSPA_IS_HINT_DEFAULT_LOW (hints[port].HintDescriptor)) {
			start = min * 0.75 + max * 0.25;
		} else if (LADSPA_IS_HINT_DEFAULT_MIDDLE (hints[port].HintDescriptor)) {
			start = min * 0.5 + max * 0.5;
		} else if (LADSPA_IS_HINT_DEFAULT_HIGH (hints[port].HintDescriptor)) {
			start = min * 0.25 + max * 0.75;
		} else if (LADSPA_IS_HINT_DEFAULT_MAXIMUM (hints[port].HintDescriptor)) {
			start = max;
		} else if (LADSPA_IS_HINT_DEFAULT_0 (hints[port].HintDescriptor)) {
			start = 0.0;
		} else if (LADSPA_IS_HINT_DEFAULT_1 (hints[port].HintDescriptor)) {
			start = 1.0;
		} else if (LADSPA_IS_HINT_DEFAULT_100 (hints[port].HintDescriptor)) {
			start = 100.0;
		} else if (LADSPA_IS_HINT_DEFAULT_440 (hints[port].HintDescriptor)) {
			start = 440.0;
		} else if (LADSPA_IS_HINT_BOUNDED_BELOW (hints[port].HintDescriptor)
			   && LADSPA_IS_HINT_BOUNDED_ABOVE (hints[port].HintDescriptor)) {
			start = (min + max) * 0.5;
		} else {
			start = 0;
		}
		
		_controls[port] = start;
	}

	for (int port = 0; port < _num_ports; ++port) {
		if (LADSPA_IS_PORT_AUDIO (_descriptor->PortDescriptors[port])) {
			_buffers[port] = (LADSPA_Data *) calloc (buffer_size, sizeof (LADSPA_Data));
			_descriptor->connect_port (_handle, port, _buffers[port]);
		}
	}
}

void
LadspaPlugin::run (int N)
{
	_descriptor->run (_handle, N);
}

int
LadspaPlugin::input_buffers () const
{
	int n = 0;
	
	for (int port = 0; port < _num_ports; ++port) {
		if (LADSPA_IS_PORT_AUDIO (_descriptor->PortDescriptors[port])) {
			if (LADSPA_IS_PORT_INPUT (_descriptor->PortDescriptors[port])) {
				++n;
			}
		}
	}
	
	return n;
}
	

float *
LadspaPlugin::input_buffer (int n) const
{
	for (int port = 0; port < _num_ports; ++port) {
		if (LADSPA_IS_PORT_AUDIO (_descriptor->PortDescriptors[port])) {
			if (LADSPA_IS_PORT_INPUT (_descriptor->PortDescriptors[port])) {
				if (n == 0) {
					return _buffers[port];
				}
				--n;
			}
		}
	}
	
	return 0;
}
