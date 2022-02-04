NEURON {
  SUFFIX all
  NONSPECIFIC_CURRENT i
  USEION k READ ek, ki WRITE ik
  USEION na READ ena, nai WRITE ina
}

STATE { naChan_gates_m_q naChan_gates_h_q kChan_gates_n_q }

INITIAL {
  LOCAL kChan_gates_n_reverseRate_r, kChan_gates_n_forwardRate_x, kChan_gates_n_forwardRate_r, kChan_gates_n_inf, naChan_gates_h_reverseRate_r, naChan_gates_h_forwardRate_r, naChan_gates_h_inf, naChan_gates_m_reverseRate_r, naChan_gates_m_forwardRate_x, naChan_gates_m_forwardRate_r, naChan_gates_m_inf

  kChan_gates_n_reverseRate_r = 0.125 * exp(-0.0125 * (65 + v))
  kChan_gates_n_forwardRate_x = 0.1 * (55 + v)
  if (kChan_gates_n_forwardRate_x != 0) { kChan_gates_n_forwardRate_r = 0.10000000149011612 * kChan_gates_n_forwardRate_x * (1 + -1 * exp(-1 * kChan_gates_n_forwardRate_x))^-1 }
  else {
  if (kChan_gates_n_forwardRate_x == 0) { kChan_gates_n_forwardRate_r = 0.10000000149011612 }
  }
  kChan_gates_n_inf = kChan_gates_n_forwardRate_r * (kChan_gates_n_forwardRate_r + kChan_gates_n_reverseRate_r)^-1
  naChan_gates_h_reverseRate_r = (1 + exp(-0.1 * (35 + v)))^-1
  naChan_gates_h_forwardRate_r = 0.07000000029802322 * exp(-0.05 * (65 + v))
  naChan_gates_h_inf = naChan_gates_h_forwardRate_r * (naChan_gates_h_forwardRate_r + naChan_gates_h_reverseRate_r)^-1
  naChan_gates_m_reverseRate_r = 4 * exp(-0.05555555555555555 * (65 + v))
  naChan_gates_m_forwardRate_x = 0.1 * (40 + v)
  if (naChan_gates_m_forwardRate_x != 0) { naChan_gates_m_forwardRate_r = naChan_gates_m_forwardRate_x * (1 + -1 * exp(-1 * naChan_gates_m_forwardRate_x))^-1 }
  else {
  if (naChan_gates_m_forwardRate_x == 0) { naChan_gates_m_forwardRate_r = 1 }
  }
  naChan_gates_m_inf = naChan_gates_m_forwardRate_r * (naChan_gates_m_forwardRate_r + naChan_gates_m_reverseRate_r)^-1
  naChan_gates_m_q = naChan_gates_m_inf
  naChan_gates_h_q = naChan_gates_h_inf
  kChan_gates_n_q = kChan_gates_n_inf
}

DERIVATIVE dstate {
  LOCAL kChan_gates_n_reverseRate_r, kChan_gates_n_forwardRate_x, kChan_gates_n_forwardRate_r, kChan_gates_n_inf, kChan_gates_n_tau, naChan_gates_h_forwardRate_r, naChan_gates_h_reverseRate_r, naChan_gates_h_tau, naChan_gates_h_inf, naChan_gates_m_reverseRate_r, naChan_gates_m_forwardRate_x, naChan_gates_m_forwardRate_r, naChan_gates_m_tau, naChan_gates_m_inf

  kChan_gates_n_reverseRate_r = 0.125 * exp(-0.0125 * (65 + v))
  kChan_gates_n_forwardRate_x = 0.1 * (55 + v)
  if (kChan_gates_n_forwardRate_x != 0) { kChan_gates_n_forwardRate_r = 0.10000000149011612 * kChan_gates_n_forwardRate_x * (1 + -1 * exp(-1 * kChan_gates_n_forwardRate_x))^-1 }
  else {
  if (kChan_gates_n_forwardRate_x == 0) { kChan_gates_n_forwardRate_r = 0.10000000149011612 }
  }
  kChan_gates_n_inf = kChan_gates_n_forwardRate_r * (kChan_gates_n_forwardRate_r + kChan_gates_n_reverseRate_r)^-1
  kChan_gates_n_tau = (kChan_gates_n_forwardRate_r + kChan_gates_n_reverseRate_r)^-1
  naChan_gates_h_forwardRate_r = 0.07000000029802322 * exp(-0.05 * (65 + v))
  naChan_gates_h_reverseRate_r = (1 + exp(-0.1 * (35 + v)))^-1
  naChan_gates_h_tau = (naChan_gates_h_forwardRate_r + naChan_gates_h_reverseRate_r)^-1
  naChan_gates_h_inf = naChan_gates_h_forwardRate_r * (naChan_gates_h_forwardRate_r + naChan_gates_h_reverseRate_r)^-1
  naChan_gates_m_reverseRate_r = 4 * exp(-0.05555555555555555 * (65 + v))
  naChan_gates_m_forwardRate_x = 0.1 * (40 + v)
  if (naChan_gates_m_forwardRate_x != 0) { naChan_gates_m_forwardRate_r = naChan_gates_m_forwardRate_x * (1 + -1 * exp(-1 * naChan_gates_m_forwardRate_x))^-1 }
  else {
  if (naChan_gates_m_forwardRate_x == 0) { naChan_gates_m_forwardRate_r = 1 }
  }
  naChan_gates_m_tau = (naChan_gates_m_forwardRate_r + naChan_gates_m_reverseRate_r)^-1
  naChan_gates_m_inf = naChan_gates_m_forwardRate_r * (naChan_gates_m_forwardRate_r + naChan_gates_m_reverseRate_r)^-1
  naChan_gates_m_q' = (naChan_gates_m_inf + -1 * naChan_gates_m_q) * naChan_gates_m_tau^-1
  naChan_gates_h_q' = (naChan_gates_h_inf + -1 * naChan_gates_h_q) * naChan_gates_h_tau^-1
  kChan_gates_n_q' = (kChan_gates_n_inf + -1 * kChan_gates_n_q) * kChan_gates_n_tau^-1
}

BREAKPOINT {
  SOLVE dstate METHOD cnexp
  LOCAL g_naChan, g_passiveChan, g_kChan


  g_passiveChan = 0.0003
  g_naChan = 0.12 * naChan_gates_h_q * naChan_gates_m_q * naChan_gates_m_q * naChan_gates_m_q
  g_kChan = 0.036 * kChan_gates_n_q *kChan_gates_n_q * kChan_gates_n_q * kChan_gates_n_q

  ik = g_kChan * (v + -1 * ek)
  i = g_passiveChan * (v + 54.387)
  ina = g_naChan * (v + -1 * ena)
}

