import React from "react";
import { ThemeContext } from "../components/ThemeProvider";
import { IconProps } from "./IconProps";

export const Copy: React.FunctionComponent<IconProps> = (props) => {
  const theme = React.useContext(ThemeContext);
  return (
    <svg width={props.width} height={props.height} viewBox="0 0 64 64">
      <g fillRule="evenodd" fill="currentColor" transform="translate(-1,-4)">
        <path d="M 10,8 H 44 V 16 H 40 V 12 H 14 v 36 h 4 v 4 H 10 Z" />
        <g>
          <path d="m 26,24 h 26 v 28 h -8 v 8 H 26 Z m -4,-4 h 34 v 35 l -9,9 H 22 Z" />
          <g fill={theme.violet}>
            <path d="m 38,28 h 10 v 4 H 38 Z" />
            <path d="m 30,36 h 18 v 4 H 30 Z" />
            <path d="m 30,44 h 18 v 4 H 30 Z" />
            <path d="m 40,52 v 4 H 30 v -4 z" />
          </g>
        </g>
      </g>
    </svg>
  );
};
