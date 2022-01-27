NEURON {
  SUFFIX sm
  NONSPECIFIC_CURRENT i
  USEION na WRITE ina
  USEION k WRITE ik
}

STATE { na_gates_m_q na_gates_h_q k_gates_n_q }

BREAKPOINT {
  SOLVE dstate METHOD cnexp
  LOCAL g_na, g_k

  g_k = 0.036 * k_gates_n_q * k_gates_n_q * k_gates_n_q * k_gates_n_q
  ik = g_k * (v + 77)

  g_na = 0.12 * na_gates_h_q * na_gates_m_q * na_gates_m_q * na_gates_m_q
  ina = g_na * (v - 50)

  i = 0.0003 * (v + 54.387)
}

INITIAL {
  LOCAL na_gates_h_reverseRate_r, na_gates_h_forwardRate_r, na_gates_m_reverseRate_r, na_gates_m_forwardRate_x, na_gates_m_forwardRate_r
  LOCAL k_gates_n_reverseRate_r, k_gates_n_forwardRate_x, k_gates_n_forwardRate_r

  na_gates_h_forwardRate_r = 0.07000000029802322 * exp(-0.05 * (65 + v))
  na_gates_h_reverseRate_r = 1/(1 + exp(-0.1 * (35 + v)))
  na_gates_m_reverseRate_r = 4 * exp(-0.05555555555555555 * (65 + v))
  na_gates_m_forwardRate_x = 0.1 * (40 + v)
  na_gates_m_forwardRate_r = 1
  if (na_gates_m_forwardRate_x != 0) {
    na_gates_m_forwardRate_r = na_gates_m_forwardRate_x / (1 - exp(-na_gates_m_forwardRate_x))
  }
  na_gates_m_q = na_gates_m_forwardRate_r / (na_gates_m_forwardRate_r + na_gates_m_reverseRate_r)
  na_gates_h_q = na_gates_h_forwardRate_r / (na_gates_h_forwardRate_r + na_gates_h_reverseRate_r)

  k_gates_n_reverseRate_r = 0.125 * exp(-0.0125 * (65 + v))
  k_gates_n_forwardRate_x = 0.1 * (55 + v)
  k_gates_n_forwardRate_r = 0.10000000149011612
  if (k_gates_n_forwardRate_x != 0) {
    k_gates_n_forwardRate_r = 0.10000000149011612 * k_gates_n_forwardRate_x / (1 - exp(-k_gates_n_forwardRate_x))
  }
  k_gates_n_q = k_gates_n_forwardRate_r / (k_gates_n_forwardRate_r + k_gates_n_reverseRate_r)
}

DERIVATIVE dstate {
  LOCAL na_gates_h_forwardRate_r, na_gates_h_reverseRate_r, na_gates_m_reverseRate_r, na_gates_m_forwardRate_x, na_gates_m_forwardRate_r
  LOCAL k_gates_n_reverseRate_r, k_gates_n_forwardRate_x, k_gates_n_forwardRate_r

  na_gates_h_forwardRate_r = 0.07000000029802322 * exp(-0.05 * (65 + v))
  na_gates_h_reverseRate_r = 1/(1 + exp(-0.1 * (35 + v)))
  na_gates_m_reverseRate_r = 4 * exp(-0.05555555555555555 * (65 + v))
  na_gates_m_forwardRate_x = 0.1 * (40 + v)
  na_gates_m_forwardRate_r = 1
  if (na_gates_m_forwardRate_x != 0) {
    na_gates_m_forwardRate_r = na_gates_m_forwardRate_x / (1 - exp(-na_gates_m_forwardRate_x))
  }

  k_gates_n_reverseRate_r = 0.125 * exp(-0.0125 * (65 + v))
  k_gates_n_forwardRate_x = 0.1 * (55 + v)
  k_gates_n_forwardRate_r = 0.10000000149011612
  if (k_gates_n_forwardRate_x != 0) {
    k_gates_n_forwardRate_r = 0.10000000149011612 * k_gates_n_forwardRate_x / (1 - exp(-k_gates_n_forwardRate_x))
  }

  na_gates_m_q' = na_gates_m_forwardRate_r - na_gates_m_q*(na_gates_m_forwardRate_r + na_gates_m_reverseRate_r)
  na_gates_h_q' = na_gates_h_forwardRate_r - na_gates_h_q*(na_gates_h_forwardRate_r + na_gates_h_reverseRate_r)
  k_gates_n_q'  = k_gates_n_forwardRate_r  - k_gates_n_q *(k_gates_n_forwardRate_r  + k_gates_n_reverseRate_r)
}
