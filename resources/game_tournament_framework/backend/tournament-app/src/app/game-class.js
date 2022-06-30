export class Game {
  /**
   * @type number
   */
  height = 0;
  /**
   * @type number
   */
  width = 0;
  /**
   * @type [[number, number]]
   */
  moves = [];
  /**
   * @type [{type: "initial" | "move" | "reaction", action: any, cells: number[]}]
   */
  state = [];


  /**
   * 
   * @param {ArrayBuffer} buffer 
   */
  constructor(buffer) {
    const arr = new Uint8Array(buffer);

    if (arr.length == 0) throw "No game data supplied"

    this.version = arr[0];

    const readValReg = i => [arr[i], arr[i + 1]];
    const readValRev = i => [arr[i + 1], arr[i]];

    let readVal;
    let addMove;
    switch (this.version) {
      case 1:
        readVal = readValRev;
        addMove = Array.prototype.unshift;
        break;
      case 2:
        readVal = readValReg;
        addMove = Array.prototype.unshift;
        break;
      case 3:
        readVal = readValRev;
        addMove = Array.prototype.push;
        break;
      case 4:
        readVal = readValReg;
        addMove = Array.prototype.push;
        break;
      default:
        throw "Invalid data version " + this.version;
    }

    [this.width, this.height] = readVal(1);
    this.moves = [];
    for (let i = 3; i < arr.length; i += 2) {
      addMove.call(this.moves, readVal(i));
    }

    this.initialState();
    let player = 1;
    for (const m of this.moves) {
      this.applyMove(m, player);
      player *= -1;
    }
  }

  initialState() {
    this.state = [
      {
        type: "initial",
        action: undefined,
        cells: new Array(this.width * this.height).fill(0)
      }
    ];
  }

  applyMove([x, y], player) {
    this.state.push({
      type: "move",
      action: {
        player: player,
        pos: [x, y]
      },
      cells: this.update((px, py, v) => px == x && py == y ? this.updateValue(v, 1, player) : v)
    });
    this.applyReactions();
  }

  applyReactions(count = 1000) {
    if (count == 0) return; // reaction timeout
    let updates = [];
    let reactions = []
    let player = 0;
    this.forEach((x, y, v) => {
      let nb = this.neighbours(x, y);
      if (nb.length <= Math.abs(v)) {
        reactions.push([x, y]);
        updates.push([x, y, -nb.length]);
        updates.push(...nb.map(([x, y]) => [x, y, 1]));
        player = Math.sign(v);
      }
    });
    if (player == 0) return;

    let state = [... this.lastState.cells];
    for (const [x, y, d] of updates) {
      let i = x + this.width * y;
      state[i] = this.updateValue(state[i], d, player);
    }

    this.state.push({ type: "reaction", action: reactions, cells: state })

    if (!this.lastState.cells.some(v => Math.sign(v) === -player)) // won
      return;

    this.applyReactions(count - 1);
  }

  get lastState() {
    return this.state[this.state.length - 1];
  }

  update(func) {
    let state = [... this.lastState.cells];
    for (let i = 0; i < state.length; i++) {
      state[i] = func(i % this.width, Math.floor(i / this.width), state[i]);
    }
    return state;
  }

  forEach(func, num = this.state.length - 1) {
    let state = this.state[num].cells;
    for (let i = 0; i < state.length; i++) {
      func(i % this.width, Math.floor(i / this.width), state[i]);
    }
  }

  updateValue(v, delta, player) {
    return (Math.abs(v) + delta) * player;
  }

  neighbours(x, y) {
    let nb = []
    if (x != 0) nb.push([x - 1, y]);
    if (y != 0) nb.push([x, y - 1]);
    if (x != (this.width - 1)) nb.push([x + 1, y]);
    if (y != (this.height - 1)) nb.push([x, y + 1]);
    return nb;
  }
}
