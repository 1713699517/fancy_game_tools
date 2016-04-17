{{class}} = class("{{class}}", ProtoBasePack)

function {{class}}:ctor()
    {{class}}.super.ctor(self)
{{members}}
end

function {{class}}:pack()
    local _ba = self._byteData
    self:fill(_ba)
    return self._byteData
end

function {{class}}:fill(_ba)
{{fills}}
end
