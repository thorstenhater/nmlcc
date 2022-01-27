NEURON {
  SUFFIX kChan
  USEION k WRITE ik
}

STATE { gates_n_q }

INITIAL {
  LOCAL gates_n_reverseRate_r, gates_n_forwardRate_x, gates_n_forwardRate_r

  gates_n_reverseRate_r = 0.125 * exp(-0.0125 * (65 + v))
  gates_n_forwardRate_x = 0.1 * (55 + v)
  gates_n_forwardRate_r = 0.10000000149011612
  if (gates_n_forwardRate_x != 0) {
    gates_n_forwardRate_r = 0.10000000149011612 * gates_n_forwardRate_x / (1 - exp(-gates_n_forwardRate_x))
  }
  gates_n_q = gates_n_forwardRate_r / (gates_n_forwardRate_r + gates_n_reverseRate_r)
}

DERIVATIVE dstate {
  LOCAL gates_n_reverseRate_r, gates_n_forwardRate_x, gates_n_forwardRate_r

  gates_n_reverseRate_r = 0.125 * exp(-0.0125 * (65 + v))
  gates_n_forwardRate_x = 0.1 * (55 + v)
  gates_n_forwardRate_r = 0.10000000149011612
  if (gates_n_forwardRate_x != 0) {
    gates_n_forwardRate_r = 0.10000000149011612 * gates_n_forwardRate_x / (1 - exp(-gates_n_forwardRate_x))
  }
  gates_n_q' = gates_n_forwardRate_r - gates_n_q * (gates_n_forwardRate_r + gates_n_reverseRate_r)
}

BREAKPOINT {
  SOLVE dstate METHOD cnexp
  LOCAL gates_n_fcond, g

  gates_n_fcond = gates_n_q*gates_n_q
  g = 0.036*gates_n_fcond*gates_n_fcond
  ik = g * (v + 77)
}

