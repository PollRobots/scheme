export class Jiffies {
  private readonly epoch: number;
  private microsPerJiffy: number;

  private constructor(epoch: number, microsPerJiffy: number) {
    this.epoch = epoch;
    this.microsPerJiffy = microsPerJiffy;
    console.log(`One jiffy is ${microsPerJiffy}μs`);
  }

  get jiffiesPerSecond(): number {
    return 1000000 / this.microsPerJiffy;
  }

  get current(): number {
    return Math.round(
      (1000 * (performance.now() - this.epoch)) / this.microsPerJiffy
    );
  }

  static init(): Jiffies {
    const intervals = new Set<number>();

    let last = performance.now();
    for (let i = 0; i < 100 && intervals.size < 2; i++) {
      const curr = performance.now();
      intervals.add(Math.round(1000 * (curr - last)));
      last = curr;
    }

    let smallest = Infinity;
    for (const interval of intervals) {
      if (interval > 0 && interval < smallest) {
        smallest = interval;
      }
    }

    return new Jiffies(performance.now(), smallest);
  }
}
