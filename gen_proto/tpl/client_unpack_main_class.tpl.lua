{{class}} = class("{{class}}", ProtoBaseUnpack)

function {{class}}:ctor()
    {{class}}.super.ctor(self)
{{members}}
end

function {{class}}:unpack(bytes)
    self._byteData:writeBuf(bytes):setPos(1)
    self:fill(self._byteData)
end

function {{class}}:fill(_ba)
{{fills}}
end
