VIIRF (Versatile IIR Filter)
This provides VHDL code and a Python configuration script for a versatile IIR filter implementation. 
Hence, the VIIRF can implement any transfer function (highpass, lowpass etc.) that can be represented as a cascade of second-order sections (SOS). 

The hardware core can provide pipelining (or not, depending on your needs). 

The configuration script takes the (floating-point) SOS (and gain-matrix G, if required) and configures the (quantized) filter. It also simulates a step-response and generates testbench-files. 

Have fun!

