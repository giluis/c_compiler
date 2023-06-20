pub struct TokenIter<TToken> {
    tokens: Vec<TToken>
}

impl <TToken> TokenIter<TToken> 
where TToken: Parsable<TToken>{

    pub fn parse<P>(&mut self ) -> Result<P, String> 
    where P: Parsable<TToken>{
        P::parse(self)
    }

    pub fn parse_if_match<P, F: Fn(&P::ApplyMatchTo) -> bool>(
         &mut self,
        _matches: F,
    ) -> Result<P, String>
    where
        Self: Sized,
        P: Parsable<TToken>
    {
        todo!()
    }
}

pub trait Parsable<TToken>
where
    TToken: Parsable<TToken>,
    Self: Sized,
{
    type ApplyMatchTo: Parsable<TToken> = Self;

    fn parse(iter: &mut TokenIter<TToken>) -> Result<Self, String>;

    fn parse_if_match<F: Fn(&Self::ApplyMatchTo) -> bool>(
        iter: &mut TokenIter<TToken>,
        _matches: F,
    ) -> Result<Self, String>
    where
        Self: Sized,
    {
        todo!()
    }

    fn identifier() -> String {
        std::any::type_name::<Self>().to_string()
    }
}

impl <T, P> Parsable<T> for Option<P>
where P: Parsable<T, ApplyMatchTo = P>, T: Parsable<T> {
    type ApplyMatchTo = P;
    fn parse(iter: &mut TokenIter<T>) -> Result<Self, String> {
        let p = iter.parse()?;
        Ok(Some(p))
    }

    fn parse_if_match<F: Fn(&Self::ApplyMatchTo) -> bool>(
            iter: &mut TokenIter<T>,
            matches: F,
        ) -> Result<Self, String>
        where
            Self: Sized, {
                let p = iter.parse_if_match(matches)?;
                Ok(Some(p))
        
    }
}

impl Parsable<char> for char {
    type ApplyMatchTo = Self;

    fn parse(iter: &mut TokenIter<char>) -> Result<Self, String> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::TokenIter;

    #[test]
    fn test1(){
        let mut iter = TokenIter{tokens: vec!['a', 'b', 'c']};
        let result = iter.parse_if_match(|tok| matches!(tok, 'a'));

    }

}
