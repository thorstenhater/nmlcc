NEURON {
  SUFFIX passiveChan
  NONSPECIFIC_CURRENT i
  RANGE e, conductance
}

PARAMETER {
  e = 0 (mV)
  conductance = 0.00000001 (mS)
}

BREAKPOINT {
  i = conductance * (v + -1 * e)
}

