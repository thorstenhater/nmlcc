NEURON {
  SUFFIX kChan
  RANGE conductance
}

PARAMETER {
  conductance = 0.00001 (uS)
}

STATE { gates_n_q }

INITIAL {
  LOCAL gates_n_reverseRate_r, gates_n_forwardRate_x, gates_n_forwardRate_r, gates_n_inf

  gates_n_reverseRate_r = 0.125 * exp(-0.0125 * (65 + v))
  gates_n_forwardRate_x = 0.1 * (55 + v)
  if (gates_n_forwardRate_x != 0) { gates_n_forwardRate_r = 0.10000000149011612 * gates_n_forwardRate_x * (1 + -1 * exp(-1 * gates_n_forwardRate_x))^-1 }
  else {
  if (gates_n_forwardRate_x == 0) { gates_n_forwardRate_r = 0.10000000149011612 }
  }
  gates_n_inf = gates_n_forwardRate_r * (gates_n_forwardRate_r + gates_n_reverseRate_r)^-1
  gates_n_q = gates_n_inf
}

DERIVATIVE dstate {
  LOCAL gates_n_reverseRate_r, gates_n_forwardRate_x, gates_n_forwardRate_r, gates_n_tau, gates_n_inf

  gates_n_reverseRate_r = 0.125 * exp(-0.0125 * (65 + v))
  gates_n_forwardRate_x = 0.1 * (55 + v)
  if (gates_n_forwardRate_x != 0) { gates_n_forwardRate_r = 0.10000000149011612 * gates_n_forwardRate_x * (1 + -1 * exp(-1 * gates_n_forwardRate_x))^-1 }
  else {
  if (gates_n_forwardRate_x == 0) { gates_n_forwardRate_r = 0.10000000149011612 }
  }
  gates_n_tau = (gates_n_forwardRate_r + gates_n_reverseRate_r)^-1
  gates_n_inf = gates_n_forwardRate_r * (gates_n_forwardRate_r + gates_n_reverseRate_r)^-1
  gates_n_q' = (gates_n_inf + -1 * gates_n_q) * gates_n_tau^-1
}

BREAKPOINT {
  SOLVE dstate METHOD cnexp

}

