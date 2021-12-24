import React from "react";
import { ThemeContext } from "./ThemeProvider";

interface FlyoutProps {
  label?: string;
  fontSize?: number;
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
  const height = (30 * label.length) / 2;
  const labelHeight = `${(1.5 * height) / 20}em`;

  return (
    <div
      style={{
        ...kFlyoutStyle,
        pointerEvents: state.expanded ? undefined : "none",
        fontSize: props.fontSize,
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
            minHeight: `calc(2em + ${labelHeight})`,
            background: theme.boldBackground,
            color: theme.foreground,
            borderStyle: "solid",
            borderWidth: "1px 0 1px 1px",
            borderColor: theme.base00,
            marginLeft: -1,
            borderRadius: "0 0 0 0.5em",
          }}
        >
          {props.children}
        </div>
      </div>
    </div>
  );
};
