import React from "react";
import { SchemeRuntime } from "../SchemeRuntime";

export const SchemeRuntimeContext = React.createContext<
  SchemeRuntime | undefined
>(undefined);

export const SchemeRuntimeProvider = SchemeRuntimeContext.Provider;
