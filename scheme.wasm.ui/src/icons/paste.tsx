import React from "react";
import { ThemeContext } from "../components/ThemeProvider";
import { IconProps } from "./IconProps";

export const Paste: React.FunctionComponent<IconProps> = (props) => {
  const theme = React.useContext(ThemeContext);
  return (
    <svg width={props.width} height={props.height} viewBox="0 0 64 64">
      <g transform="translate(-64, -2)" fillRule="evenodd" fill="currentColor">
        <g>
          <path d="m 90,24 h 26 v 28 h -8 v 8 H 90 Z m -4,-4 h 34 v 35 l -9,9 H 86 Z" />
          <g fill={theme.violet}>
            <path d="m 102,28 h 10 v 4 h -10 z" />
            <path d="m 94,36 h 18 v 4 H 94 Z" />
            <path d="m 94,44 h 18 v 4 H 94 Z" />
            <path d="m 104,52 v 4 H 94 v -4 z" />
          </g>
        </g>
        <path
          d="m 97,8 a 2,2 0 0 1 -2,2 2,2 0 0 1 -2,-2 2,2 0 0 1 2,-2 2,2 0 0 1 2,2 z
          M 95,4 C 90.581235,4.0004883 87,5.7903726 87,7.999512 h -9
          c -2.20914,0 -4.000488,1.7913488 -4.000488,4.000488 V 56
          C 73.999512,58.208651 75.79086,60 78,60 h 4 V 56 H 78 V 12 h 9
          c 0,2.209139 1.791353,4.000488 4.000485,4.000488 H 99
          c 2.20914,0 4.00048,-1.791349 4.00048,-4.000488 h 9 v 4.000488
          h 4 V 12 c 0,-2.2091392 -1.78989,-4.000488 -3.99904,-4.000488 h -9
          c 0,-2.2091394 -3.582692,-3.9990237 -8.000965,-3.9990237 z"
        />
      </g>
    </svg>
  );
};
