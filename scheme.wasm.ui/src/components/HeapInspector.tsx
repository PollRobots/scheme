import React, { useEffect } from "react";
import { Theme } from "../monaco/theme";
import { SchemeRuntimeContext } from "./SchemeRuntimeProvider";
import { ThemeContext } from "./ThemeProvider";
import { ToggleSwitch } from "./ToggleSwitch";

interface HeapInspectorProps {
  scale?: number;
}
interface HeapInspectorState {
  ptr: string;
  lookupRes: string;
  counter: number;
  showEmpty: boolean;
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
  props
) => {
  const runtime = React.useContext(SchemeRuntimeContext);
  const theme = React.useContext(ThemeContext);
  const [state, setState] = React.useState<HeapInspectorState>({
    ptr: "",
    lookupRes: "",
    counter: 0,
    showEmpty: false,
  });

  if (!runtime || runtime.stopped) {
    return (
      <div style={{ margin: "1em", color: theme.base00 }}>
        Runtime not available
      </div>
    );
  }

  function onLookup(ptr: number) {
    if (!runtime) {
      return;
    }
    const words = new Uint32Array(runtime.memory.buffer.slice(ptr, ptr + 12));
    const type = words[0] & 0x1f;
    const output: string[] = [];
    if (type == 0) {
      output.push("<empty>");
    } else {
      const listener = (str: string) => {
        output.push(str);
      };
      runtime.addEventListener("write-priority", listener);
      runtime.print(ptr);
      runtime.removeEventListener("write-priority", listener);
    }
    setState({
      ...state,
      ptr: ptr.toString(16),
      lookupRes: `${ptr.toString(16)}(${type}): ${output.join("")}`,
    });
  }

  // const heaps: { ptr: number; size: number; free: number; next: number }[] = [];
  const heaps: React.ReactNode[] = [];

  let ptr = runtime.gHeap;

  while (ptr != 0) {
    const words = new Uint32Array(runtime.memory.buffer.slice(ptr, ptr + 12));
    const size = words[0];
    const free = words[1];
    const nextHeap = words[2];

    heaps.push(
      <div style={{ textAlign: "right" }} key={`ptr${ptr}`}>
        {ptr.toString(16)}
      </div>
    );
    heaps.push(
      <div style={{ textAlign: "right" }} key={`size${ptr}`}>
        {size}
      </div>
    );
    heaps.push(
      <div style={{ textAlign: "right" }} key={`free${ptr}`}>
        {free.toString(16)}
      </div>
    );
    heaps.push(
      <div style={{ textAlign: "right" }} key={`next${ptr}`}>
        {nextHeap.toString(16)}
      </div>
    );
    if (state.showEmpty || !isHeapEmpty(ptr, size, runtime.memory)) {
      heaps.push(
        <HeapView
          key={`cvs${ptr}`}
          ptr={ptr}
          size={size}
          scale={props.scale || 1}
          width={512}
          onLookup={(ptr) => onLookup(ptr)}
        />
      );
    }
    ptr = nextHeap;
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
            onChange={(on) => setState({ ...state, showEmpty: on })}
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
          onClick={() => {
            const output: string[] = [];
            const listener = (str: string) => {
              output.push(str);
            };
            runtime.addEventListener("write-priority", listener);
            runtime.gcRun(runtime.replEnv);
            runtime.removeEventListener("write-priority", listener);
            console.log(output.join(""));
            if (output.length) {
              console.log(output.join(""));
            }
            setState({ ...state, counter: state.counter + 1 });
          }}
        >
          gc
        </button>

        <div>{runtime.gGcIsCollecting ? "active" : "idle"} </div>

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
          {runtime.gGcCollectedCount}{" "}
          <Percentage
            val={runtime.gGcCollectedCount}
            total={runtime.gGcCollectedCount + runtime.gGcNotCollectedCount}
          />
        </div>

        <div style={{ justifySelf: "end" }}>
          {runtime.gGcNotCollectedCount}{" "}
          <Percentage
            val={runtime.gGcNotCollectedCount}
            total={runtime.gGcCollectedCount + runtime.gGcNotCollectedCount}
          />
        </div>
        <div style={{ justifySelf: "end" }}>
          {runtime.gGcCollectedCount + runtime.gGcNotCollectedCount}
        </div>

        <div style={{ gridColumnStart: 1 }}>
          {runtime.gGcCollectionCount} Collections
        </div>
        <div style={{ justifySelf: "end" }}>
          {runtime.gGcTotalCollectedCount}{" "}
          <Percentage
            val={runtime.gGcTotalCollectedCount}
            total={
              runtime.gGcTotalCollectedCount + runtime.gGcTotalNotCollectedCount
            }
          />
        </div>

        <div style={{ justifySelf: "end" }}>
          {runtime.gGcTotalNotCollectedCount}{" "}
          <Percentage
            val={runtime.gGcTotalNotCollectedCount}
            total={
              runtime.gGcTotalCollectedCount + runtime.gGcTotalNotCollectedCount
            }
          />
        </div>
        <div style={{ justifySelf: "end" }}>
          {runtime.gGcTotalCollectedCount + runtime.gGcTotalNotCollectedCount}
        </div>
      </div>
    </div>
  );
};

function isHeapEmpty(
  ptr: number,
  size: number,
  memory: WebAssembly.Memory
): boolean {
  const words = new Uint32Array(
    memory.buffer.slice(ptr, ptr + 12 * (1 + size))
  );
  for (let i = 1; i <= size; i++) {
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

  useEffect(() => {
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
    const words = new Uint32Array(
      runtime.memory.buffer.slice(
        props.ptr + 12,
        props.ptr + 12 * (1 + props.size)
      )
    );

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
      switch (type) {
        case 1: // nil
          color = theme.base00;
          break;
        case 2: // boolean
          color = theme.base00;
          break;
        case 3: // cons
          fill = false;
          color = theme.red;
          break;
        case 4: // i64
          color = theme.violet;
          break;
        case 5: // f64
          color = theme.blue;
          break;
        case 6: // symbol
          color = theme.yellow;
          break;
        case 7: // string
          color = theme.green;
          break;
        case 8: // char
          color = theme.orange;
          break;
        case 9: // env
          fill = false;
          color = theme.blue;
          break;
        case 10: // special
          color = theme.base1;
          break;
        case 11: // builtin
          color = theme.base1;
          break;
        case 12: // lambda
          color = theme.magenta;
        case 13: // error
          color = theme.red;
          break;
        case 14: // values
          fill = false;
          color = theme.green;
          break;
        case 15: // vector
          color = theme.blue;
          break;
        case 16: // bytevector
          color = theme.violet;
          break;
        case 17: // cont
          color = theme.base1;
          break;
        case 18: // big-int
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
