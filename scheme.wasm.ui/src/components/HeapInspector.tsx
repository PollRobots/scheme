import React from "react";
import { Theme } from "../monaco/theme";
import { SchemeType } from "../SchemeType";
import { SchemeRuntimeContext } from "./SchemeRuntimeProvider";
import { ThemeContext } from "./ThemeProvider";
import { ToggleSwitch } from "./ToggleSwitch";

interface HeapInspectorProps {
  scale?: number;
  clock: number;
}

interface HeapDefinition {
  ptr: number;
  size: number;
  free: number;
  next: number;
  entries: ArrayBuffer;
}

interface HeapInspectorState {
  ptr: string;
  lookupRes: string;
  counter: number;
  showEmpty: boolean;
}

interface GcStatistics {
  isCollecting: boolean;
  collectionCount: number;
  collected: number;
  notCollected: number;
  totalCollected: number;
  totalNotCollected: number;
}

function buttonStyle(theme: Theme, disabled?: boolean): React.CSSProperties {
  return {
    margin: "0.25em",
    alignSelf: "start",
    background: theme.blue,
    borderColor: theme.base00,
    color: theme.boldForeground,
    opacity: disabled ? 0.7 : 1,
    borderWidth: 1,
    borderStyle: "solid",
    borderRadius: "0.25em",
    minWidth: "4em",
    padding: "0.25em",
  };
}

export const HeapInspector: React.FunctionComponent<HeapInspectorProps> = (
  props: HeapInspectorProps
) => {
  const runtime = React.useContext(SchemeRuntimeContext);
  const theme = React.useContext(ThemeContext);
  const [state, setState] = React.useState<HeapInspectorState>({
    ptr: "",
    lookupRes: "",
    counter: 0,
    showEmpty: false,
  });
  const [heapState, setHeaps] = React.useState<HeapDefinition[]>([]);
  const [gcStatistics, setGcStatistics] = React.useState<GcStatistics>({
    isCollecting: false,
    collectionCount: 0,
    collected: 0,
    notCollected: 0,
    totalCollected: 0,
    totalNotCollected: 0,
  });
  React.useEffect(() => {
    if (runtime) {
      runtime.gcRun(false).then((stats) => {
        setGcStatistics(stats);
        lookupHeaps(runtime.heap, heapState);
      });
    }
  }, [props.clock, state.counter]);

  if (!runtime || runtime.stopped) {
    return (
      <div style={{ margin: "1em", color: theme.base00 }}>
        Runtime not available
      </div>
    );
  }

  async function onLookup(ptr: number) {
    if (!runtime) {
      return;
    }
    // find the relevant heap
    const heap = heapState.find(
      (el) => ptr >= el.ptr + 12 && ptr < el.ptr + 12 * (1 + el.size)
    );
    if (!heap) {
      return;
    }
    const idx = (ptr - (heap.ptr + 12)) / 12;
    if (idx != (idx | 0)) {
      return;
    }
    const words = new Uint32Array(heap.entries);
    const type = words[idx * 3] & SchemeType.Mask;
    const output: string[] = [];
    if (type == SchemeType.Empty) {
      output.push("<empty>");
    } else {
      const listener = (str: string) => {
        output.push(str);
      };
      output.push(await runtime.print(ptr));
    }
    setState({
      ...state,
      ptr: ptr.toString(16),
      lookupRes: `${ptr.toString(16)}(${
        SchemeType[type]
      }:${type}): ${output.join("")}`,
    });
  }

  async function lookupHeaps(ptr: number, heaps: HeapDefinition[]) {
    if (ptr == 0 || !runtime) {
      return;
    }

    const heap = await runtime.getHeap(ptr);
    const idx = heaps.findIndex((el) => el.ptr === heap.ptr);
    if (idx >= 0) {
      heaps[idx] = heap;
    } else {
      heaps.push(heap);
    }
    if (heap.next) {
      lookupHeaps(heap.next, heaps);
    } else {
      setHeaps(heaps);
    }
  }

  const heaps: React.ReactNode[] = [];

  for (const heap of heapState) {
    heaps.push(
      <div style={{ textAlign: "right" }} key={`ptr${heap.ptr}`}>
        {heap.ptr.toString(16)}
      </div>,
      <div style={{ textAlign: "right" }} key={`size${heap.ptr}`}>
        {heap.size}
      </div>,
      <div style={{ textAlign: "right" }} key={`free${heap.ptr}`}>
        {heap.free.toString(16)}
      </div>,
      <div style={{ textAlign: "right" }} key={`next${heap.ptr}`}>
        {heap.next.toString(16)}
      </div>
    );
    if (state.showEmpty || !isHeapEmpty(heap)) {
      heaps.push(
        <HeapView
          key={`cvs${heap.ptr}`}
          ptr={heap.ptr}
          size={heap.size}
          entries={heap.entries}
          scale={props.scale || 1}
          width={512}
          onLookup={(ptr) => onLookup(ptr)}
        />
      );
    }
  }

  const kColumnHeapStyle: React.CSSProperties = {
    background: theme.blue,
    textAlign: "center",
  };

  return (
    <div
      style={{
        padding: "0.5em",
        maxWidth: 64 * Math.round(8 * (props.scale || 1) + 2),
      }}
    >
      <div
        style={{
          display: "grid",
          columnGap: 1,
          rowGap: 1,
          gridTemplateColumns: "1fr 1fr 1fr 1fr",
          maxHeight: "75vh",
          overflowY: "auto",
          color: theme.foreground,
        }}
      >
        <div style={{ fontWeight: 500 }}>Memory</div>
        <div
          style={{ gridColumnStart: 2, gridColumnEnd: 5, justifySelf: "end" }}
        >
          Show empty slabs{" "}
          <ToggleSwitch
            on={state.showEmpty}
            onChange={(on: boolean) => setState({ ...state, showEmpty: on })}
          />
        </div>
        <div style={kColumnHeapStyle}>Address</div>
        <div style={kColumnHeapStyle}>Size</div>
        <div style={kColumnHeapStyle}>FreePtr</div>
        <div style={kColumnHeapStyle}>Next</div>
        {heaps}
      </div>
      <div style={{ display: "grid", gridTemplateColumns: "auto auto 1fr" }}>
        <input
          style={{
            margin: "0.25em",
            alignSelf: "start",
            width: "4em",
            borderColor: theme.base00,
            background: theme.background,
            color: theme.foreground,
          }}
          type="text"
          pattern="[0-9a-zA-Z]+"
          value={state.ptr}
          onChange={(e) => {
            setState({ ...state, ptr: e.target.value });
          }}
        />
        <button
          disabled={state.ptr === ""}
          style={buttonStyle(theme, state.ptr === "")}
          onClick={() => {
            let ptr = 0;
            if (state.ptr.match(/^[0-9]+$/)) {
              ptr = Number(state.ptr);
            } else if (state.ptr.match(/^[0-9a-fA-F]+$/)) {
              ptr = parseInt(state.ptr, 16);
            } else {
              return;
            }
            onLookup(ptr);
          }}
        >
          lookup
        </button>
        <span
          style={{
            margin: "0.25em",
            border: "1px solid",
            background: theme.background,
            borderColor: theme.base00,
            maxHeight: "4em",
            lineHeight: "1em",
            overflowY: "auto",
            color: theme.foreground,
          }}
        >
          {state.lookupRes}
        </span>
      </div>
      <div
        style={{
          display: "grid",
          columnGap: "0.25em",
          rowGap: "0.25em",
          gridTemplateColumns: "auto 1fr 1fr 1fr 1fr auto",
          fontSize: "smaller",
          color: theme.foreground,
        }}
      >
        <div
          style={{
            fontSize: "larger",
            gridColumnStart: 1,
            gridColumnEnd: 6,
            fontWeight: 500,
            alignSelf: "end",
          }}
        >
          GC stats
        </div>
        <button
          style={{ ...buttonStyle(theme), justifySelf: "end" }}
          onClick={async () => {
            const gcResp = await runtime.gcRun(true);
            console.log(gcResp.output);
            setState({ ...state, counter: state.counter + 1 });
            setGcStatistics(gcResp);
          }}
        >
          gc
        </button>

        <div>{gcStatistics.isCollecting ? "active" : "idle"} </div>

        <div
          style={{ gridColumnStart: 2, fontWeight: 500, justifySelf: "end" }}
        >
          Collected
        </div>
        <div
          style={{ gridColumnStart: 3, fontWeight: 500, justifySelf: "end" }}
        >
          Kept
        </div>
        <div
          style={{ gridColumnStart: 4, fontWeight: 500, justifySelf: "end" }}
        >
          Total
        </div>

        <div style={{ gridColumnStart: 1 }}>Last Collection</div>
        <div style={{ justifySelf: "end" }}>
          {gcStatistics.collected}{" "}
          <Percentage
            val={gcStatistics.collected}
            total={gcStatistics.collected + gcStatistics.notCollected}
          />
        </div>

        <div style={{ justifySelf: "end" }}>
          {gcStatistics.notCollected}{" "}
          <Percentage
            val={gcStatistics.notCollected}
            total={gcStatistics.collected + gcStatistics.notCollected}
          />
        </div>
        <div style={{ justifySelf: "end" }}>
          {gcStatistics.collected + gcStatistics.notCollected}
        </div>

        <div style={{ gridColumnStart: 1 }}>
          {gcStatistics.collectionCount} Collections
        </div>
        <div style={{ justifySelf: "end" }}>
          {gcStatistics.totalCollected}{" "}
          <Percentage
            val={gcStatistics.totalCollected}
            total={gcStatistics.totalCollected + gcStatistics.totalNotCollected}
          />
        </div>

        <div style={{ justifySelf: "end" }}>
          {gcStatistics.totalNotCollected}{" "}
          <Percentage
            val={gcStatistics.totalNotCollected}
            total={gcStatistics.totalCollected + gcStatistics.totalNotCollected}
          />
        </div>
        <div style={{ justifySelf: "end" }}>
          {gcStatistics.totalCollected + gcStatistics.totalNotCollected}
        </div>
      </div>
    </div>
  );
};

function isHeapEmpty(heap: HeapDefinition): boolean {
  const words = new Uint32Array(heap.entries);
  for (let i = 0; i < heap.size; i++) {
    if ((words[i * 3] & 0x1f) != 0) {
      return false;
    }
  }
  return true;
}

const Percentage: React.FunctionComponent<{
  val: number;
  total: number;
  round?: number;
}> = (props) => {
  if (props.total == 0) {
    return <span>--%</span>;
  }
  const pct = (100 * props.val) / props.total;
  const display =
    props.round === undefined ? Math.round(pct) : pct.toFixed(props.round);
  return <span>{display}%</span>;
};

interface HeapViewProps {
  ptr: number;
  size: number;
  scale: number;
  width: number;
  entries: ArrayBuffer;
  onLookup?: (ptr: number) => void;
}

const HeapView: React.FunctionComponent<HeapViewProps> = (props) => {
  const runtime = React.useContext(SchemeRuntimeContext);
  const theme = React.useContext(ThemeContext);
  const canvasRef = React.useRef<HTMLCanvasElement>(null);

  if (!runtime || runtime.stopped) {
    return null;
  }

  const cellSize = Math.round(8 * props.scale);

  React.useEffect(() => {
    if (!canvasRef.current || !runtime || runtime.stopped) {
      return;
    }

    const ctx = canvasRef.current.getContext("2d");
    if (!ctx) {
      return;
    }
    ctx.resetTransform();
    ctx.clearRect(0, 0, canvasRef.current.width, canvasRef.current.height);
    ctx.translate(0.5, 0.5);
    const words = new Uint32Array(props.entries);

    for (let i = 0; i < props.size; i++) {
      const offset = i * 3;
      const type = words[offset] & 0x1f;
      if (type == 0) {
        continue;
      }
      const x = i % 64;
      const y = i >> 6;

      let fill = true;
      let color = "";
      switch (type as SchemeType) {
        case SchemeType.Nil: // nil
          color = theme.base00;
          break;
        case SchemeType.Boolean: // boolean
          color = theme.base00;
          break;
        case SchemeType.Cons: // cons
          fill = false;
          color = theme.red;
          break;
        case SchemeType.I64: // i64
          color = theme.violet;
          break;
        case SchemeType.F64: // f64
          color = theme.blue;
          break;
        case SchemeType.Symbol: // symbol
          color = theme.yellow;
          break;
        case SchemeType.Str: // string
          color = theme.green;
          break;
        case SchemeType.Char: // char
          color = theme.orange;
          break;
        case SchemeType.Env: // env
          fill = false;
          color = theme.blue;
          break;
        case SchemeType.Special: // special
          color = theme.base1;
          break;
        case SchemeType.Builtin: // builtin
          color = theme.base1;
          break;
        case SchemeType.Lambda: // lambda
          color = theme.magenta;
          break;
        case SchemeType.Error: // error
          color = theme.red;
          break;
        case SchemeType.Values: // values
          fill = false;
          color = theme.green;
          break;
        case SchemeType.Vector: // vector
          fill = false;
          color = theme.blue;
          break;
        case SchemeType.Bytevector: // bytevector
          fill = false;
          color = theme.violet;
          break;
        case SchemeType.Cont: // cont
          color = theme.base1;
          break;
        case SchemeType.BigInt: // big-int
          color = theme.cyan;
          break;
        case SchemeType.Except:
          color = theme.base2;
          break;
        case SchemeType.ContProc:
          color = theme.base1;
          break;
        case SchemeType.SyntaxRules:
          color = theme.orange;
          break;
        case SchemeType.Rational:
          color = theme.cyan;
          break;
        default:
          continue;
      }
      if (fill) {
        ctx.fillStyle = color;
        ctx.fillRect(x * cellSize, y * cellSize, cellSize - 1, cellSize - 1);
      } else {
        ctx.strokeStyle = color;
        ctx.lineWidth = 1;
        ctx.strokeRect(x * cellSize, y * cellSize, cellSize - 2, cellSize - 2);
      }
    }
  });

  function onMouseUp(evt: React.MouseEvent<HTMLCanvasElement>) {
    if (!canvasRef.current) {
      return;
    }
    const rect = canvasRef.current.getBoundingClientRect();
    const x = evt.clientX - rect.x;
    const y = evt.clientY - rect.y;
    const index = Math.floor(x / cellSize) + Math.floor(y / cellSize) * 64;
    const ptr = props.ptr + 12 + index * 12;
    if (props.onLookup) {
      props.onLookup(ptr);
    }
  }

  return (
    <canvas
      onMouseUp={(e) => onMouseUp(e)}
      ref={canvasRef}
      style={{
        gridColumnStart: 1,
        gridColumnEnd: 5,
        borderWidth: 1,
        borderStyle: "solid",
        borderColor: theme.base00,
      }}
      key={`cvs${props.ptr}`}
      width={cellSize * 64}
      height={cellSize * ((props.size + 63) >> 6)}
    />
  );
};
