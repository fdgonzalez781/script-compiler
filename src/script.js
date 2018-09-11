function i(x) {
  return x;
}

function k(x, y) {
  return x;
}

function s(x, y, z) {
  return x(z)(y(z));
}

function f() {
  return i(i);
}

function g() {
  return s(k)(k);
}

function zero(f, x) {
  return x;
}

function one(f, x) {
  return f(x);
}

function succ(n, f, x) {
  return f(n(f)(x));
}

function plus(m, n, f, x) {
  return m(f)(n(f)(x));
}

function times(m, n, f, x) {
  return m(n(f))(x);
}

function pow(m, n, f, x) {
  return n(m)(f)(x);
}

function bt(x, y) {
  return x;
}

function bf(x, y) {
  return y;
}

function ifElse(b, t, f) {
  return b(t)(f);
}

function and(p, q) {
  return ifElse(p)(q)(bf);
}

function or(p, q) {
  return ifElse(p)(bt)(q);
}

function y(f) {
  return function(x) {
  return f(x(x));
}
(function(x) {
  return f(x(x));
}
);
}

