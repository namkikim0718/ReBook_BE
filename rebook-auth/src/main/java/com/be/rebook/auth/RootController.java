package com.be.rebook.auth;

import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.bind.annotation.GetMapping;

/**
 * 테스트용 컨트롤러 입니다! 언제든 삭제해도 좋습니다.
 */
@RestController
public class RootController {

    @GetMapping
    public String ROOT_PATH() {
        return "<h1> 루트 페이지다! <h1>";
    }

}
