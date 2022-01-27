NEURON {
  SUFFIX naChan
  USEION na READ ena WRITE ina
  RANGE conductance
}

PARAMETER {
  conductance = 0.00000001 (mS)
}

STATE { gates_m_q gates_h_q }

INITIAL {
  LOCAL gates_h_reverseRate_r, gates_h_forwardRate_r, gates_m_reverseRate_r, gates_m_forwardRate_x, gates_m_forwardRate_r

  gates_h_forwardRate_r = 0.07000000029802322 * exp(-0.05 * (65 + v))
  gates_h_reverseRate_r = 1/(1 + exp(-0.1 * (35 + v)))
  gates_m_reverseRate_r = 4 * exp(-0.05555555555555555 * (65 + v))
  gates_m_forwardRate_x = 0.1 * (40 + v)
  gates_m_forwardRate_r = 1
  if (gates_m_forwardRate_x != 0) {
    gates_m_forwardRate_r = gates_m_forwardRate_x / (1 - exp(-gates_m_forwardRate_x))
  }
  gates_m_q = gates_m_forwardRate_r / (gates_m_forwardRate_r + gates_m_reverseRate_r)
  gates_h_q = gates_h_forwardRate_r / (gates_h_forwardRate_r + gates_h_reverseRate_r)
}

DERIVATIVE dstate {
  LOCAL gates_h_forwardRate_r, gates_h_reverseRate_r, gates_m_reverseRate_r, gates_m_forwardRate_x, gates_m_forwardRate_r

  gates_h_forwardRate_r = 0.07000000029802322 * exp(-0.05 * (65 + v))
  gates_h_reverseRate_r = 1/(1 + exp(-0.1 * (35 + v)))
  gates_m_reverseRate_r = 4 * exp(-0.05555555555555555 * (65 + v))
  gates_m_forwardRate_x = 0.1 * (40 + v)
  gates_m_forwardRate_r = 1
  if (gates_m_forwardRate_x != 0) {
    gates_m_forwardRate_r = gates_m_forwardRate_x / (1 - exp(-gates_m_forwardRate_x))
  }
  gates_m_q' = gates_m_forwardRate_r - gates_m_q*(gates_m_forwardRate_r + gates_m_reverseRate_r)
  gates_h_q' = gates_h_forwardRate_r - gates_h_q*(gates_h_forwardRate_r + gates_h_reverseRate_r)
}

BREAKPOINT {
  SOLVE dstate METHOD cnexp
  LOCAL g

  g = conductance * gates_h_q * gates_m_q * gates_m_q * gates_m_q
  ina = g * (v - ena)
}

