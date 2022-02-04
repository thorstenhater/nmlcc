NEURON {
  SUFFIX naChan
  RANGE conductance
}

PARAMETER {
  conductance = 0.00001 (uS)
}

STATE { gates_m_q gates_h_q }

INITIAL {
  LOCAL gates_h_reverseRate_r, gates_h_forwardRate_r, gates_h_inf, gates_m_reverseRate_r, gates_m_forwardRate_x, gates_m_forwardRate_r, gates_m_inf

  gates_h_reverseRate_r = (1 + exp(-0.1 * (35 + v)))^-1
  gates_h_forwardRate_r = 0.07000000029802322 * exp(-0.05 * (65 + v))
  gates_h_inf = gates_h_forwardRate_r * (gates_h_forwardRate_r + gates_h_reverseRate_r)^-1
  gates_m_reverseRate_r = 4 * exp(-0.05555555555555555 * (65 + v))
  gates_m_forwardRate_x = 0.1 * (40 + v)
  if (gates_m_forwardRate_x != 0) { gates_m_forwardRate_r = gates_m_forwardRate_x * (1 + -1 * exp(-1 * gates_m_forwardRate_x))^-1 }
  else {
  if (gates_m_forwardRate_x == 0) { gates_m_forwardRate_r = 1 }
  }
  gates_m_inf = gates_m_forwardRate_r * (gates_m_forwardRate_r + gates_m_reverseRate_r)^-1
  gates_m_q = gates_m_inf
  gates_h_q = gates_h_inf
}

DERIVATIVE dstate {
  LOCAL gates_h_reverseRate_r, gates_h_forwardRate_r, gates_h_tau, gates_h_inf, gates_m_reverseRate_r, gates_m_forwardRate_x, gates_m_forwardRate_r, gates_m_tau, gates_m_inf

  gates_h_reverseRate_r = (1 + exp(-0.1 * (35 + v)))^-1
  gates_h_forwardRate_r = 0.07000000029802322 * exp(-0.05 * (65 + v))
  gates_h_tau = (gates_h_forwardRate_r + gates_h_reverseRate_r)^-1
  gates_h_inf = gates_h_forwardRate_r * (gates_h_forwardRate_r + gates_h_reverseRate_r)^-1
  gates_m_reverseRate_r = 4 * exp(-0.05555555555555555 * (65 + v))
  gates_m_forwardRate_x = 0.1 * (40 + v)
  if (gates_m_forwardRate_x != 0) { gates_m_forwardRate_r = gates_m_forwardRate_x * (1 + -1 * exp(-1 * gates_m_forwardRate_x))^-1 }
  else {
  if (gates_m_forwardRate_x == 0) { gates_m_forwardRate_r = 1 }
  }
  gates_m_tau = (gates_m_forwardRate_r + gates_m_reverseRate_r)^-1
  gates_m_inf = gates_m_forwardRate_r * (gates_m_forwardRate_r + gates_m_reverseRate_r)^-1
  gates_m_q' = (gates_m_inf + -1 * gates_m_q) * gates_m_tau^-1
  gates_h_q' = (gates_h_inf + -1 * gates_h_q) * gates_h_tau^-1
}

BREAKPOINT {
  SOLVE dstate METHOD cnexp

}

