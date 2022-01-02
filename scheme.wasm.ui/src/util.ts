export function reference(...els: any[]) {}

export async function saveFilePicker(text: string) {
  const blob = new Blob([text], {
    type: "text/plain",
  });

  if (!Reflect.has(window, "showSaveFilePicker")) {
    return saveFallback(blob);
  }

  const handle = await showSaveFilePicker({
    suggestedName: "untitled.scm",
    types: [
      {
        description: "Scheme files",
        accept: {
          "text/x-scheme": [".scm"],
        },
      },
    ],
  });

  const writable = await handle.createWritable();
  await writable.write(blob);
  await writable.close();
}

function saveFallback(blob: Blob) {
  const anchor = document.createElement("a");
  anchor.href = window.URL.createObjectURL(blob);
  anchor.download = "untitled.scm";
  anchor.click();
}

export async function openFilePicker(): Promise<string> {
  let text = "";
  if (!Reflect.has(window, "showOpenFilePicker")) {
    return openFallback();
  }

  const [handle] = await showOpenFilePicker({
    types: [
      {
        description: "Scheme file",
        accept: {
          "text/x-scheme": [".scm"],
        },
      },
    ],
    multiple: false,
  });

  const file = await handle.getFile();
  return file.text();
}

function openFallback(): Promise<string> {
  return new Promise<string>((resolve, reject) => {
    const input = document.createElement("input");
    input.type = "file";
    input.multiple = false;
    input.accept = ".scm,text/plain,text/x-script.scheme,text/x-scheme";
    const timeout = window.setTimeout(
      () => reject(new Error("Timed out opening file")),
      10 * 60 * 1000 // times out after 10 minutes
    );
    input.addEventListener("change", async () => {
      const files = input.files;
      if (!files || files.length == 0) {
        return reject(new Error("No files"));
      }
      const file = files.item(0);
      if (!file) {
        return reject(new Error("No File"));
      }
      window.clearTimeout(timeout);
      resolve(await file.text());
    });

    input.click();
  });
}
