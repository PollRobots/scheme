import React from "react";
import { RuntimeWorker } from "../RuntimeWorker";

export const SchemeRuntimeContext = React.createContext<
  RuntimeWorker | undefined
>(undefined);

export const SchemeRuntimeProvider = SchemeRuntimeContext.Provider;
