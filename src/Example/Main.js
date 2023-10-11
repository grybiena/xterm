export const onLoad = go => () => {
  document.onreadystatechange = function () {
     if (document.readyState == "complete") {
       go(document)();
   }
 }
}
