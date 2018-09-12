function i(x) {
  return x;
}

function k(x) {
  return function(y) {
  return x;
}
;
}

function s(x) {
  return function(y) {
  return function(z) {
  return x(z)(y(z));
}
;
}
;
}

function f() {
  return i(i);
}

function g() {
  return s(k)(k);
}

function zero(f) {
  return function(x) {
  return x;
}
;
}

function one(f) {
  return function(x) {
  return f(x);
}
;
}

function succ(n) {
  return function(f) {
  return function(x) {
  return f(n(f)(x));
}
;
}
;
}

function plus(m) {
  return function(n) {
  return function(f) {
  return function(x) {
  return m(f)(n(f)(x));
}
;
}
;
}
;
}

function times(m) {
  return function(n) {
  return function(f) {
  return function(x) {
  return m(n(f))(x);
}
;
}
;
}
;
}

function pow(m) {
  return function(n) {
  return function(f) {
  return function(x) {
  return n(m)(f)(x);
}
;
}
;
}
;
}

function bt(x) {
  return function(y) {
  return x;
}
;
}

function bf(x) {
  return function(y) {
  return y;
}
;
}

function ifElse(b) {
  return function(t) {
  return function(f) {
  return b(t)(f);
}
;
}
;
}

function and(p) {
  return function(q) {
  return ifElse(p)(q)(bf);
}
;
}

function or(p) {
  return function(q) {
  return ifElse(p)(bt)(q);
}
;
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

