Impromptu-Libraries
===================

Libraries to use with Impromptu to aid with playing different midi sounds.

Put into libraries folder of Impromptu to have wrapped functions.  Note that two instruments
will be automatically loaded whenever you open Impromptu as the functions often use
INST1 or INST2.  They are connected through a merger to *au:output-node* so to add
more instruments you must disconnect this node, add another merger, and then connect 
through that merger.  