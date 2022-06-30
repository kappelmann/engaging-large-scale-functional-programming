export class Notifier {
  #value = undefined;
  #subscriber = new Set();

  constructor(value) {
    this.#value = value;
  }

  subscribe(func) {
    this.#subscriber.add(func);
  }

  unsubscribe(func) {
    this.#subscriber.remove(func);
  }

  set value(value) {
    const prev = this.#value;
    this.#value = value;
    this.#subscriber.forEach(f => f(value, prev));
  }

  get value() {
    return this.#value;
  }
}