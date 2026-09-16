# The images playground posts the edit payload wrapped as {"form_data": {...}}, but
# /api/v1/images/edit declares an unembedded body, so FastAPI rejects it with 422 before the
# request ever reaches ComfyUI. Generation is unaffected: its call is not wrapped. Naming the
# parameter in `embed` matches what the frontend already sends. Drop once upstream reconciles
# the route it exposed in 0.11.1 with the frontend.
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
