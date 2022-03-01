import React from "react";
import { FocusContextProvider } from "./FocusContext";
import { ThemeContext } from "./ThemeProvider";

interface FlyoutProps {
  label?: string;
  fontSize?: number;
  offset?: number;
}

interface FlyoutState {
  expanded: boolean;
}

const kDefaultState: FlyoutState = {
  expanded: false,
};

const kFlyoutStyle: React.CSSProperties = {
  position: "absolute",
  top: "1rem",
  right: 0,
};

export const Flyout: React.FunctionComponent<FlyoutProps> = (props) => {
  const [state, setState] = React.useState(kDefaultState);
  const theme = React.useContext(ThemeContext);
  const label = props.label || "flyout";
  const height = 15 * label.length;
  const labelHeight = `${label.length}em`;

  return (
    <div
      style={{
        ...kFlyoutStyle,
        pointerEvents: state.expanded ? undefined : "none",
        fontSize: `${props.fontSize}pt`,
      }}
    >
      <div
        style={{
          display: "grid",
          gridTemplateColumns: "auto 1fr",
          transition: "transform 0.3s",
          transform: state.expanded
            ? "translateX(0)"
            : "translateX(calc(100% - 1.5em))",
        }}
      >
        <div
          style={{
            zIndex: 1,
            background: state.expanded ? theme.boldBackground : theme.base00,
            color: state.expanded ? theme.foreground : theme.foreground,
            borderStyle: "solid",
            borderWidth: "1px 0 1px 1px",
            borderColor: theme.base00,
            marginLeft: -1,
            borderRadius: "0.25em 0 0 0.25em",
            cursor: "pointer",
            alignSelf: "start",
            transition: "background 0.3s, color 0.5s",
            pointerEvents: "all",
            marginTop: props.offset ? `${props.offset}em` : undefined,
          }}
          onClick={() => setState({ ...state, expanded: !state.expanded })}
        >
          <svg
            viewBox={`0 0 20 ${height}`}
            style={{
              width: "1.5em",
              height: labelHeight,
            }}
          >
            <g>
              <text
                textAnchor="middle"
                transform={`rotate(90 5 ${height / 2})`}
                x="5"
                y={height / 2}
                fill="currentColor"
              >
                {props.label || "flyout"}
              </text>
            </g>
          </svg>
        </div>
        <div
          style={{
            minWidth: "10em",
            minHeight: `calc(2em + ${labelHeight} + ${props.offset || 0}em)`,
            background: theme.boldBackground,
            color: theme.foreground,
            borderStyle: "solid",
            borderWidth: "1px 0 1px 1px",
            borderColor: theme.base00,
            marginLeft: -1,
            borderRadius: "0 0 0 0.5em",
          }}
        >
          <FocusContextProvider value={state.expanded}>
            {props.children}
          </FocusContextProvider>
        </div>
      </div>
    </div>
  );
};
