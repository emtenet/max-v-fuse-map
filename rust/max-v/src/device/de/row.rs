use super::*;

pub (super) struct RowInterconnect<'v> {
    pub (super) density: &'v DensityLayout,
    pub (super) x: X,
    pub (super) y: Y,
    pub (super) i: IORowInterconnectIndex,
}

impl<'v> RowInterconnect<'v> {
    pub (super) fn normal<'de, A>(
        &self,
        a: &mut A,
        i: &'v mut Interconnect<[Source; 13]>,
    ) -> Result<(), A::Error>
    where
        A: MapAccess<'de>,
    {
        use Select3::*;
        use Select4::*;

        i.port = Port::IORowInterconnect {
            x: self.x,
            y: self.y,
            i: self.i,
        };

        self.direct_link(a, &mut i.sources[0])?;
        self.select(a, &mut i.sources[ 1], "select-0-0", Select4_0, Select3_0)?;
        self.select(a, &mut i.sources[ 2], "select-0-1", Select4_0, Select3_1)?;
        self.select(a, &mut i.sources[ 3], "select-0-2", Select4_0, Select3_2)?;
        self.select(a, &mut i.sources[ 4], "select-1-0", Select4_1, Select3_0)?;
        self.select(a, &mut i.sources[ 5], "select-1-1", Select4_1, Select3_1)?;
        self.select(a, &mut i.sources[ 6], "select-1-2", Select4_1, Select3_2)?;
        self.select(a, &mut i.sources[ 7], "select-2-0", Select4_2, Select3_0)?;
        self.select(a, &mut i.sources[ 8], "select-2-1", Select4_2, Select3_1)?;
        self.select(a, &mut i.sources[ 9], "select-2-2", Select4_2, Select3_2)?;
        self.select(a, &mut i.sources[10], "select-3-0", Select4_3, Select3_0)?;
        self.select(a, &mut i.sources[11], "select-3-1", Select4_3, Select3_1)?;
        self.select(a, &mut i.sources[12], "select-3-2", Select4_3, Select3_2)?;

        Ok(())
    }

    pub (super) fn global<'de, A>(
        &self,
        a: &mut A,
        i: &'v mut Interconnect<[Source; 16]>,
    ) -> Result<(), A::Error>
    where
        A: MapAccess<'de>,
    {
        use Select3::*;
        use Select4::*;

        i.port = Port::IORowInterconnect {
            x: self.x,
            y: self.y,
            i: self.i,
        };

        self.direct_link(a, &mut i.sources[0])?;
        self.select(a, &mut i.sources[ 1], "select-0-0", Select4_0, Select3_0)?;
        self.select(a, &mut i.sources[ 2], "select-0-1", Select4_0, Select3_1)?;
        self.select(a, &mut i.sources[ 3], "select-0-2", Select4_0, Select3_2)?;
        self.select(a, &mut i.sources[ 4], "select-1-0", Select4_1, Select3_0)?;
        self.select(a, &mut i.sources[ 5], "select-1-1", Select4_1, Select3_1)?;
        self.select(a, &mut i.sources[ 6], "select-1-2", Select4_1, Select3_2)?;
        self.select(a, &mut i.sources[ 7], "select-2-0", Select4_2, Select3_0)?;
        self.select(a, &mut i.sources[ 8], "select-2-1", Select4_2, Select3_1)?;
        self.select(a, &mut i.sources[ 9], "select-2-2", Select4_2, Select3_2)?;
        self.select(a, &mut i.sources[10], "select-3-0", Select4_3, Select3_0)?;
        self.select(a, &mut i.sources[11], "select-3-1", Select4_3, Select3_1)?;
        self.select(a, &mut i.sources[12], "select-3-2", Select4_3, Select3_2)?;
        self.select_global(a, &mut i.sources[13], "select-g-0", Select3_0)?;
        self.select_global(a, &mut i.sources[14], "select-g-1", Select3_1)?;
        self.select_global(a, &mut i.sources[15], "select-g-2", Select3_2)?;

        Ok(())
    }

    fn direct_link<'de, A>(
        &self,
        access: &mut A,
        source: &mut Source,
    ) -> Result<(), A::Error>
    where
        A: MapAccess<'de>,
    {
        source.fuse[0] = Fuse::IORowInterconnect {
            x: self.x,
            y: self.y,
            i: self.i,
            fuse: IOInterconnectFuse::DirectLink,
        }.to_index(self.density).unwrap();

        access.next_key_seed(Key("direct-link"))?;
        access.next_value_seed(SourceVisitor { source })
    }

    fn select<'de, A>(
        &self,
        access: &mut A,
        source: &mut Source,
        key: &'static str,
        select4: Select4,
        select3: Select3,
    ) -> Result<(), A::Error>
    where
        A: MapAccess<'de>,
    {
        source.fuse[0] = Fuse::IORowInterconnect {
            x: self.x,
            y: self.y,
            i: self.i,
            fuse: IOInterconnectFuse::Source4(select4),
        }.to_index(self.density).unwrap();
        source.fuse[1] = Fuse::IORowInterconnect {
            x: self.x,
            y: self.y,
            i: self.i,
            fuse: IOInterconnectFuse::Source3(select3),
        }.to_index(self.density).unwrap();

        access.next_key_seed(Key(key))?;
        access.next_value_seed(SourceVisitor { source })
    }

    fn select_global<'de, A>(
        &self,
        access: &mut A,
        source: &mut Source,
        key: &'static str,
        select3: Select3,
    ) -> Result<(), A::Error>
    where
        A: MapAccess<'de>,
    {
        source.fuse[0] = Fuse::IORowInterconnect {
            x: self.x,
            y: self.y,
            i: self.i,
            fuse: IOInterconnectFuse::SourceGlobal,
        }.to_index(self.density).unwrap();
        source.fuse[1] = Fuse::IORowInterconnect {
            x: self.x,
            y: self.y,
            i: self.i,
            fuse: IOInterconnectFuse::Source3(select3),
        }.to_index(self.density).unwrap();

        access.next_key_seed(Key(key))?;
        access.next_value_seed(SourceVisitor { source })
    }
}

