# The playground wraps the edit body in form_data, which /api/v1/images/edit rejects with 422. Drop once upstream reconciles the 0.11.1 route.
_final: prev: {
  open-webui = prev.open-webui.overrideAttrs (old: {
    postPatch = (old.postPatch or "") + ''
      substituteInPlace backend/open_webui/routers/images.py \
        --replace-fail \
          'from fastapi import APIRouter, Depends, HTTPException, Request, UploadFile' \
          'from fastapi import APIRouter, Body, Depends, HTTPException, Request, UploadFile' \
        --replace-fail \
          'async def edit_images(request: Request, form_data: EditImageForm, user=Depends(get_verified_user)):' \
          'async def edit_images(request: Request, form_data: EditImageForm = Body(..., embed=True), user=Depends(get_verified_user)):'
    '';
  });
}
