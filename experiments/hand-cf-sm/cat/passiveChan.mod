NEURON {
  SUFFIX passiveChan
  NONSPECIFIC_CURRENT i
}

BREAKPOINT {
  i = 0.0003 * (v + 54.387)
}
