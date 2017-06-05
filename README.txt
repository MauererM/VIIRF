This repository contains sources and documentation of the VIIRF (Versatile IIR Filter) project. 
The project structure should be self-explanatory; the doc-folder contains a manual.

The VIIRF can implement any transfer function (highpass, lowpass etc.) that can be represented as a cascade of second-order sections (SOS). 

The configuration script takes the (floating-point) SOS (and gain-matrix G, if required) and configures the (quantized) filter. It also simulates a step-response and generates testbench-files. 

Have fun!

