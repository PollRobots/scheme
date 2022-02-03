import React from "react";
import { ThemeContext } from "../components/ThemeProvider";
import { IconProps } from "./IconProps";

export const Save: React.FunctionComponent<IconProps> = (props) => {
  const theme = React.useContext(ThemeContext);
  return (
    <svg width={props.width} height={props.height} viewBox="0 0 64 64">
      <g fillRule="evenodd" transform="translate(-202, -2)">
        <g fill="currentColor">
          <g>
            <path d="m 250,29 h -32 v -4 h 32 z" />
            <path d="m 250,18 h -32 v -4 h 32 z" />
          </g>
          <path
            d="m 254,8 h -40 v 28 h 40 z m 6,56 c 3,0 4,-1 4,-4 V 8
             c 0,-3 -1,-4 -4,-4 h -52 c -3,0 -4,1 -4,4 v 48 c 0,2 0,2 1,3 l 4,4
             c 1,1 1,1 3,1 h 6 V 40 h 34 v 24 z"
          />
        </g>
        <path
          fill={theme.violet}
          d="m 236,60 h -8 V 48 h 8 z m 12,-16 h -26 v 20 h 26 z"
        />
      </g>
    </svg>
  );
};
